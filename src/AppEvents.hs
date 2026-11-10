module AppEvents where

import Brick
import Brick.BChan (BChan, writeBChan)
import Brick.Types (zoom)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Control.Monad (when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put, modify)
import Control.Exception (SomeException, try)
import qualified Data.Text.Zipper as Z
import Lens.Micro ((^.))
import System.Exit (ExitCode(..))
import System.Info (os)
import System.Process (readProcessWithExitCode)
import Control.Concurrent (forkIO, threadDelay)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath (replaceExtension, takeExtension, dropExtension)
import Data.Char (isDigit, toLower)
import Data.List (sort, elemIndex, intercalate)
import Data.Maybe (listToMaybe, fromMaybe)

import AppState
import Config (loadConfig, defaultConfigText, readFileStrict, cfgDefaultObjectColor, cfgExportResolution, cfgFilter, cfgVideoFps, cfgAutoCompileDelayMs, IronConfig)
import AppCore
import Selection (deleteRange, selectedText, normalisePaste)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Write `code` to `path`, recompile/preview it, and record it in the
--   recent-files list. Shared by Ctrl+S, the unsaved-changes prompt, and
--   the Save dialog, which each layer on their own mode/state transitions.
writeAndCompile :: AppState -> FilePath -> String -> IO (AppStatus, [FilePath])
writeAndCompile st path code = do
    writeFile path code
    newErr <- compileAndSave (cfgDefaultObjectColor (_config st)) True code
    newRecents <- saveRecent path (_recentFiles st)
    let newStatus = case newErr of
            Nothing -> Saved
            Just (e, lineNum) -> ErrorMsg e lineNum
    return (newStatus, newRecents)

-- | Load `path`'s contents into the editor, compile/preview it, and switch
--   to Editing mode. Shared by the splash screen's recent-files list and
--   the Open dialog.
openFile :: AppState -> FilePath -> EventM Name AppState ()
openFile st path = do
    absPath <- liftIO $ makeAbsolute path
    content <- liftIO $ readFile path
    _ <- liftIO $ compileAndSave (cfgDefaultObjectColor (_config st)) True content
    newRecents <- liftIO $ saveRecent absPath (_recentFiles st)
    put (st { _mode = Editing
            , _currentFile = Just absPath
            , _editor = E.editor CodeEditor Nothing content
            , _status = Normal
            , _recentFiles = newRecents
            , _isDirty = False
            , _undoStack = []
            , _redoStack = []
            , _pendingUndoSnapshot = True
            , _selAnchor = Nothing
            , _mouseDragging = False
            })

-- | GLOBAL ROUTER
handleEvent :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()

-- Intercept the custom timer event
handleEvent _ (AppEvent (CompileTimerFired version)) = do
    st <- get
    -- Only compile if the user hasn't typed anything since this timer started
    when (version == _editVersion st) $ do
        let code = unlines $ E.getEditContents (_editor st)
        newErr <- liftIO $ compileAndSave (cfgDefaultObjectColor (_config st)) False code
        let newStatus = case newErr of
                Nothing -> Normal
                Just (e, lineNum) -> ErrorMsg e lineNum
        put (st { _status = newStatus })

-- The user paused for undoGroupingMs with no further edits, so the *next*
-- edit starts a brand-new undo step (see handleEditorInput).
handleEvent _ (AppEvent (UndoBoundary version)) = do
    st <- get
    when (version == _editVersion st) $
        put (st { _pendingUndoSnapshot = True })

-- Progress updates from a background export
handleEvent _ (AppEvent (ExportProgress p)) = do
    st <- get
    put (st { _exportProgress = Just p })

-- Background export finished (successfully or with an error)
handleEvent _ (AppEvent (ExportFinished result)) = do
    st <- get
    let newStatus = case result of
            Right objPath      -> Exported objPath
            Left (e, lineNum)  -> ErrorMsg e lineNum
    put (st { _exportProgress = Nothing, _status = newStatus })

handleEvent _ (VtyEvent (V.EvKey (V.KChar 'q') [V.MCtrl])) = halt -- Global Killswitch
handleEvent chan ev = do
    st <- get
    case _mode st of
        Splash        -> handleSplash chan ev
        Editing       -> handleEditing chan ev
        SaveDialog    -> handleSaveDialog chan ev
        OpenDialog    -> handleOpenDialog chan ev
        UnsavedPrompt -> handleUnsavedPrompt chan ev
        ConfigEditing -> handleConfigEditing chan ev
        CommandPalette -> handleCommandPalette chan ev

-- | 1. SPLASH SCREEN
handleSplash :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleSplash _ (VtyEvent (V.EvKey V.KEsc []))   = halt
handleSplash _ (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    put (st { _mode = Editing, _editor = E.editor CodeEditor Nothing "", _currentFile = Nothing, _status = Normal, _isDirty = False
            , _undoStack = [], _redoStack = [], _pendingUndoSnapshot = True
            , _selAnchor = Nothing, _mouseDragging = False })
handleSplash _ (VtyEvent (V.EvKey (V.KChar 'o') [])) = do
    st <- get
    put (st { _mode = OpenDialog, _status = Normal, _openInput = E.editor OpenEditor (Just 1) "" })
handleSplash _ (VtyEvent (V.EvKey (V.KChar 'g') [])) = do
    st <- get
    configPath <- liftIO getConfigPath
    exists <- liftIO $ doesFileExist configPath
    content <- if exists
                   then liftIO $ readFileStrict configPath
                   else return defaultConfigText
    put (st { _mode = ConfigEditing
            , _configInput = E.editor ConfigEditor Nothing content
            })
handleSplash _ (VtyEvent (V.EvKey (V.KChar c) [])) 
    | c `elem` ['1'..'5'] = do
        st <- get
        let idx = read [c] - 1
            recents = _recentFiles st
        when (idx < length recents) $ do
            let path = recents !! idx
            exists <- liftIO $ doesFileExist path
            when exists $ openFile st path
handleSplash _ _ = return ()

-- | 2. EDITING SCREEN
handleEditing :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEditing _ (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    if _isDirty st
        then put (st { _mode = UnsavedPrompt })
        else put (st { _mode = Splash })

-- COMMAND DISPATCH: check the incoming key against the configured
-- keybindings (Ctrl+P, Ctrl+S, Ctrl+O, Ctrl+G, Ctrl+E, Ctrl+R by default).
-- Anything that doesn't match falls through to FlyMode movement keys or
-- normal editor typing.
handleEditing chan brickEv@(VtyEvent ev@(V.EvKey key mods)) = do
    st <- get
    case lookupCommand (_config st) key mods of
        Just cmdId -> runCommand chan cmdId
        Nothing
            | isMovementKey ev -> handleMovementKey chan brickEv ev
            | otherwise         -> handleEditorInput chan brickEv

-- Catch-all for non-key events (resize, paste, etc.)
handleEditing chan ev = handleEditorInput chan ev

-- INTERCEPT MOVEMENT KEYS (For FlyMode)
isMovementKey :: V.Event -> Bool
isMovementKey (V.EvKey V.KUp []) = True
isMovementKey (V.EvKey V.KDown []) = True
isMovementKey (V.EvKey V.KLeft []) = True
isMovementKey (V.EvKey V.KRight []) = True
isMovementKey (V.EvKey (V.KChar c) []) = c `elem` ['w', 'a', 's', 'd', 'z', 'x', 'W', 'A', 'S', 'D', 'Z', 'X']
isMovementKey _ = False

handleMovementKey :: BChan CustomEvent -> BrickEvent Name CustomEvent -> V.Event -> EventM Name AppState ()
handleMovementKey chan brickEv ev = do
    st <- get
    if _viewerMode st == FlyMode
        then do
            let cmd = case ev of
                        V.EvKey V.KUp []         -> "CMD:PITCH_UP"
                        V.EvKey (V.KChar 'w') [] -> "CMD:PITCH_UP"
                        V.EvKey V.KDown []       -> "CMD:PITCH_DOWN"
                        V.EvKey (V.KChar 's') [] -> "CMD:PITCH_DOWN"
                        V.EvKey V.KLeft []       -> "CMD:YAW_LEFT"
                        V.EvKey (V.KChar 'a') [] -> "CMD:YAW_LEFT"
                        V.EvKey V.KRight []      -> "CMD:YAW_RIGHT"
                        V.EvKey (V.KChar 'd') [] -> "CMD:YAW_RIGHT"
                        V.EvKey (V.KChar 'z') [] -> "CMD:ZOOM_IN"
                        V.EvKey (V.KChar 'x') [] -> "CMD:ZOOM_OUT"
                        V.EvKey (V.KChar 'W') [] -> "CMD:PAN_FORWARD"
                        V.EvKey (V.KChar 'S') [] -> "CMD:PAN_BACKWARD"
                        V.EvKey (V.KChar 'A') [] -> "CMD:PAN_LEFT"
                        V.EvKey (V.KChar 'D') [] -> "CMD:PAN_RIGHT"
                        V.EvKey (V.KChar 'Z') [] -> "CMD:PAN_UP"
                        V.EvKey (V.KChar 'X') [] -> "CMD:PAN_DOWN"
                        _                        -> ""
            liftIO $ sendCommand cmd
            return () -- Consumes the event so it doesn't type into the editor
        else
            handleEditorInput chan brickEv -- Passes it through to type normally

-- | KEY-COMBO PARSING
--   Parses strings like "ctrl+p" or "ctrl+shift+p" into a Vty key + modifiers.
parseKeyCombo :: String -> Maybe (V.Key, [V.Modifier])
parseKeyCombo s =
    case splitOnPlus (map toLower s) of
        [] -> Nothing
        parts -> do
            mods <- mapM parseModifier (init parts)
            key  <- parseKey (last parts)
            return (key, mods)

parseModifier :: String -> Maybe V.Modifier
parseModifier "ctrl"  = Just V.MCtrl
parseModifier "alt"   = Just V.MAlt
parseModifier "meta"  = Just V.MMeta
parseModifier "shift" = Just V.MShift
parseModifier _       = Nothing

parseKey :: String -> Maybe V.Key
parseKey "esc"       = Just V.KEsc
parseKey "enter"     = Just V.KEnter
parseKey "tab"       = Just (V.KChar '\t')
parseKey "space"     = Just (V.KChar ' ')
parseKey "up"        = Just V.KUp
parseKey "down"      = Just V.KDown
parseKey "left"      = Just V.KLeft
parseKey "right"     = Just V.KRight
parseKey "backspace" = Just V.KBS
parseKey "delete"    = Just V.KDel
parseKey ('f':rest)
    | not (null rest) && all isDigit rest = Just (V.KFun (read rest))
parseKey [c] = Just (V.KChar c)
parseKey _   = Nothing

-- | Find the command (if any) bound to this key + modifier combination,
--   based on the user's configured (or default) keybindings.
lookupCommand :: IronConfig -> V.Key -> [V.Modifier] -> Maybe CommandId
lookupCommand cfg key mods =
    listToMaybe
        [ cmdId
        | cmdId <- [minBound .. maxBound]
        , Just (k, m) <- [parseKeyCombo (keybindingFor cfg cmdId)]
        , k == key, sort m == sort mods
        ]

-- | Run the action for a command, shared between keybinding dispatch and
--   the command palette.
runCommand :: BChan CustomEvent -> CommandId -> EventM Name AppState ()
runCommand _ CmdCommandPalette = do
    st <- get
    put (st { _mode = CommandPalette
            , _paletteInput = E.editor PaletteEditor (Just 1) ""
            , _paletteSelected = 0
            })

runCommand _ CmdCycleViewMode = do
    st <- get
    let nextMode = case _viewerMode st of
            OrbitMode  -> StaticMode
            StaticMode -> FlyMode
            FlyMode    -> OrbitMode
    liftIO $ sendCommand ("CMD:" ++ show nextMode)
    put (st { _viewerMode = nextMode })

runCommand _ CmdResetCamera = do
    liftIO $ sendCommand "CMD:RESET_CAMERA"
    return ()

-- Cycle through "none" plus every .glsl file in the filters directory.
-- The viewer installs the built-in examples there on first launch, and
-- any file the user drops in is picked up automatically.
runCommand _ CmdCycleFilter = do
    st <- get
    filtersDir <- liftIO getFiltersDir
    dirExists <- liftIO $ doesDirectoryExist filtersDir
    files <- if dirExists then liftIO $ listDirectory filtersDir else return []
    let names   = sort [ dropExtension f | f <- files, takeExtension f == ".glsl" ]
        options = "none" : names
        idx     = fromMaybe (-1) (elemIndex (_currentFilter st) options)
        next    = options !! ((idx + 1) `mod` length options)
    liftIO $ sendCommand ("CMD:SET_FILTER:" ++ next)
    put (st { _currentFilter = next, _status = FilterSet next })

runCommand _ CmdSettings = do
    st <- get
    configPath <- liftIO getConfigPath
    exists <- liftIO $ doesFileExist configPath
    content <- if exists
                   then liftIO $ readFileStrict configPath
                   else return defaultConfigText
    put (st { _mode = ConfigEditing
            , _configInput = E.editor ConfigEditor Nothing content
            })

runCommand _ CmdOpenFile = do
    st <- get
    put (st { _mode = OpenDialog, _status = Normal, _openInput = E.editor OpenEditor (Just 1) "" })

runCommand chan CmdExportOBJ = startMeshExport chan "obj" exportModelToOBJ

-- Coloured mesh for the web (three.js) etc.: keeps paint() and material().
runCommand chan CmdExportGLB = startMeshExport chan "glb" exportModelToGLB

-- Toggle looping playback of the script's camera(...) animation. The
-- keyframe track itself is (re)sent on every compile, so the viewer
-- already has the latest one by the time this runs.
runCommand _ CmdToggleAnimation = do
    st <- get
    if _animPlaying st
        then do
            liftIO $ sendCommand "CMD:ANIM_STOP"
            put (st { _animPlaying = False, _status = Info "Animation stopped" })
        else do
            let code = unlines $ E.getEditContents (_editor st)
            case parseAndValidate code of
                Right script | scriptHasAnimation script -> do
                    liftIO $ sendCommand "CMD:ANIM_PLAY"
                    put (st { _animPlaying = True
                            , _status = Info "Animation playing - run again to stop" })
                _ -> put (st { _status = ErrorMsg "No animation: add camera(t, yaw, pitch, dist) keyframes first" 0 })

-- Ask the viewer to render the animation to a video file next to the
-- .irsm file. The viewer picks .mp4 (ffmpeg on PATH) or .gif (built-in)
-- and reports progress in its window title / forge.log.
runCommand _ CmdExportVideo = do
    st <- get
    case _currentFile st of
        Nothing -> put (st { _status = ErrorMsg "Save the file before exporting a video" 0 })
        Just path -> do
            let code = unlines $ E.getEditContents (_editor st)
            case parseAndValidate code of
                Left (e, lineNum) -> put (st { _status = ErrorMsg e lineNum })
                Right script
                    | not (scriptHasAnimation script) ->
                        put (st { _status = ErrorMsg "No animation: add camera(t, yaw, pitch, dist) keyframes first" 0 })
                    | otherwise -> do
                        -- Re-send the scene so the export matches the editor.
                        _ <- liftIO $ compileAndSave (cfgDefaultObjectColor (_config st)) False code
                        let fps = cfgVideoFps (_config st)
                        liftIO $ sendCommand ("CMD:EXPORT_VIDEO:" ++ show fps ++ ":" ++ dropExtension path)
                        put (st { _status = Info "Video export started - see the viewer window for progress" })

-- Undo: pop the most recent snapshot, push the current text onto the redo
-- stack, and restore it as the editor content. No-op (safe) on an empty
-- undo stack. Cursor is not restored (Brick's Editor doesn't expose a
-- simple public setter for it); it lands wherever `E.editor` puts it by
-- default. Treated like any other edit for recompilation purposes, and the
-- restored text becomes its own fresh undo boundary for subsequent typing.
runCommand chan CmdUndo = do
    st <- get
    case _undoStack st of
        [] -> return () -- nothing to undo
        (prev:rest) -> do
            let current = unlines $ E.getEditContents (_editor st)
                nextVersion = _editVersion st + 1
            put (st { _editor = E.editor CodeEditor Nothing prev
                    , _undoStack = rest
                    , _redoStack = current : _redoStack st
                    , _pendingUndoSnapshot = True
                    , _selAnchor = Nothing
                    , _isDirty = True
                    , _editVersion = nextVersion
                    })
            scheduleCompile chan nextVersion

-- Redo: the mirror image of undo. No-op on an empty redo stack.
runCommand chan CmdRedo = do
    st <- get
    case _redoStack st of
        [] -> return () -- nothing to redo
        (next:rest) -> do
            let current = unlines $ E.getEditContents (_editor st)
                nextVersion = _editVersion st + 1
            put (st { _editor = E.editor CodeEditor Nothing next
                    , _redoStack = rest
                    , _undoStack = current : _undoStack st
                    , _pendingUndoSnapshot = True
                    , _selAnchor = Nothing
                    , _isDirty = True
                    , _editVersion = nextVersion
                    })
            scheduleCompile chan nextVersion

-- Copy the selection, or the whole current line when nothing is selected.
runCommand _ CmdCopy = do
    st <- get
    let txt = clipboardText st
    liftIO $ copyToSystemClipboard txt
    put (st { _clipboard = txt })

-- Cut the selection, or the whole current line when nothing is selected.
runCommand chan CmdCut = do
    st <- get
    let txt = clipboardText st
    liftIO $ copyToSystemClipboard txt
    put (st { _clipboard = txt })
    trackEdit chan $ case selectionRange st of
        Just _  -> deleteSelection
        Nothing -> do
            let ls = E.getEditContents (_editor st)
                (r, _) = E.getCursorPosition (_editor st)
                rest = case take r ls ++ drop (r + 1) ls of
                           [] -> [""]
                           xs -> xs
                ed = E.applyEdit (Z.moveCursorClosest (r, 0))
                                 (E.editor CodeEditor Nothing (intercalate "\n" rest))
            modify (\s -> s { _editor = ed })

-- Paste the internal clipboard, replacing any selection. (Most terminals
-- also paste the OS clipboard themselves via bracketed paste, which goes
-- through handleEditorInput.)
runCommand chan CmdPaste = do
    st <- get
    trackEdit chan $ do
        deleteSelection
        zoom editorLens $ modify (E.applyEdit (Z.insertMany (_clipboard st)))

runCommand _ CmdSave = do
    st <- get
    case _currentFile st of
        Just path -> do
            let code = unlines $ E.getEditContents (_editor st)
            (newStatus, newRecents) <- liftIO $ writeAndCompile st path code
            put (st { _status = newStatus, _recentFiles = newRecents, _isDirty = False })
        Nothing ->
            put (st { _mode = SaveDialog })

-- | HELPER: Runs standard editor inputs and triggers compilation
--
-- Undo/redo snapshot heuristic: pushing a full-buffer snapshot on every
-- keystroke would make undo annoyingly fine-grained (undoing one character
-- at a time). Instead we coalesce edits into 500ms bursts: a snapshot of
-- the *pre-edit* text is only pushed when `_pendingUndoSnapshot` is True,
-- which happens (a) on the very first edit after loading/opening a file,
-- and (b) when the UndoBoundary timer fires (see scheduleCompile), meaning
-- the user paused for >= undoGroupingMs. Any burst of typing/deleting
-- within one window collapses to a single undo step; pausing starts a
-- fresh boundary for the next burst. (This used to share the compile
-- debounce; they are separate so the compile delay can be much shorter.) A new edit after an undo always clears
-- the redo stack (standard undo/redo semantics).
--
-- Selection: Shift+arrows/Home/End and left-drag extend a selection from
-- `_selAnchor` to the cursor. Typing, Enter, paste, Backspace and Delete
-- replace/remove the selected text; any other key clears it.
handleEditorInput :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEditorInput chan ev = do
    st <- get
    let hasSel = selectionRange st /= Nothing
    case ev of
        -- Shift+movement: start (or keep) the anchor, then move the cursor.
        VtyEvent (V.EvKey key [V.MShift]) | Just mv <- shiftMovement key -> do
            let cur = E.getCursorPosition (_editor st)
            put (st { _selAnchor = Just (fromMaybe cur (_selAnchor st)) })
            zoom editorLens $ E.handleEditorEvent (VtyEvent (V.EvKey mv []))

        -- Left button: a fresh press places the cursor and drops the anchor
        -- there; further MouseDowns while held (terminal drag reports)
        -- move only the cursor, extending the selection.
        MouseDown CodeEditor V.BLeft _ (Location (col, row)) -> do
            let moved = E.applyEdit (Z.moveCursorClosest (row, col)) (_editor st)
                cur   = E.getCursorPosition moved
                anchor | _mouseDragging st = _selAnchor st
                       | otherwise         = Just cur
            put (st { _editor = moved, _selAnchor = anchor, _mouseDragging = True })

        MouseUp {} -> do
            -- A plain click (no drag distance) leaves no selection behind.
            let cur = E.getCursorPosition (_editor st)
                anchor = if _selAnchor st == Just cur then Nothing else _selAnchor st
            put (st { _mouseDragging = False, _selAnchor = anchor })

        -- Mouse wheel over the editor or its scroll bar. The editor keeps
        -- its cursor in view, so scrolling moves the cursor.
        MouseDown _ V.BScrollUp _ _   -> moveCursorLines (-3)
        MouseDown _ V.BScrollDown _ _ -> moveCursorLines 3

        -- Clickable vertical scroll bar: arrows step a line, the trough
        -- pages by the visible height.
        MouseDown (ScrollBar el CodeEditor) V.BLeft _ _ -> do
            page <- maybe 10 (\vp -> max 1 (snd (vp ^. vpSize) - 1)) <$> lookupViewport CodeEditor
            case el of
                SBHandleBefore -> moveCursorLines (-1)
                SBHandleAfter  -> moveCursorLines 1
                SBTroughBefore -> moveCursorLines (negate page)
                SBTroughAfter  -> moveCursorLines page
                SBBar          -> return ()

        -- Backspace/Delete with a selection: remove it, nothing else.
        VtyEvent (V.EvKey key []) | hasSel, key `elem` [V.KBS, V.KDel] ->
            trackEdit chan deleteSelection

        -- Inserting text with a selection replaces it.
        VtyEvent (V.EvKey key []) | hasSel, insertsText key ->
            trackEdit chan (deleteSelection >> passToEditor ev)
        -- Terminal paste (bracketed). Insert it ourselves: terminals send
        -- line breaks as '\r', which the editor widget would silently drop,
        -- turning a pasted multi-line script into one long line.
        VtyEvent (V.EvPaste bytes) ->
            case TE.decodeUtf8' bytes of
                Left _    -> return ()
                Right txt -> trackEdit chan $ do
                    deleteSelection
                    zoom editorLens $ modify (E.applyEdit (Z.insertMany (normalisePaste (T.unpack txt))))

        -- Any other key (plain arrows, Ctrl+K, ...) drops the selection.
        VtyEvent (V.EvKey _ _) -> trackEdit chan (clearSelection >> passToEditor ev)

        _ -> trackEdit chan (passToEditor ev)
  where
    insertsText (V.KChar c) = c /= '\t'
    insertsText V.KEnter    = True
    insertsText _           = False

-- | Shift+key -> the plain key whose movement extends the selection.
shiftMovement :: V.Key -> Maybe V.Key
shiftMovement k
    | k `elem` [V.KUp, V.KDown, V.KLeft, V.KRight, V.KHome, V.KEnd] = Just k
    | otherwise = Nothing

-- | Export the editor's script as a mesh next to the saved file (with the
--   given extension) on a background thread, reporting progress to the UI.
startMeshExport
    :: BChan CustomEvent
    -> String
    -> ((Float, Float, Float) -> String -> FilePath -> Int -> (Float -> IO ()) -> IO (Either (String, Int) FilePath))
    -> EventM Name AppState ()
startMeshExport chan ext exporter = do
    st <- get
    case _currentFile st of
        Nothing -> put (st { _status = ErrorMsg "Save the file before exporting" 0 })
        Just _ | _exportProgress st /= Nothing -> return () -- export already in progress
        Just path -> do
            let code = unlines $ E.getEditContents (_editor st)
                outPath = replaceExtension path ext
                resolution = cfgExportResolution (_config st)
                color = cfgDefaultObjectColor (_config st)
            put (st { _exportProgress = Just 0.0 })
            _ <- liftIO $ forkIO $ do
                result <- exporter color code outPath resolution
                                   (\p -> writeBChan chan (ExportProgress p))
                writeBChan chan (ExportFinished result)
            return ()

-- | What Copy/Cut take: the selection, or the current line plus newline.
clipboardText :: AppState -> String
clipboardText st = case selectionRange st of
    Just (a, b) -> selectedText ls a b
    Nothing     -> (if r < length ls then ls !! r else "") ++ "\n"
  where
    ls = E.getEditContents (_editor st)
    (r, _) = E.getCursorPosition (_editor st)

passToEditor :: BrickEvent Name CustomEvent -> EventM Name AppState ()
passToEditor ev = zoom editorLens $ E.handleEditorEvent ev

-- | The current selection as (anchor, cursor), if it is non-empty.
selectionRange :: AppState -> Maybe ((Int, Int), (Int, Int))
selectionRange st = case _selAnchor st of
    Just a | a /= cur -> Just (a, cur)
    _                 -> Nothing
  where cur = E.getCursorPosition (_editor st)

clearSelection :: EventM Name AppState ()
clearSelection = modify (\st -> st { _selAnchor = Nothing, _mouseDragging = False })

-- | Remove the selected text (if any) and put the cursor where it began.
deleteSelection :: EventM Name AppState ()
deleteSelection = do
    st <- get
    case selectionRange st of
        Nothing -> clearSelection
        Just (a, b) -> do
            let (newLines, (r, c)) = deleteRange (E.getEditContents (_editor st)) a b
                ed = E.applyEdit (Z.moveCursor (r, c)) (E.editor CodeEditor Nothing (intercalate "\n" newLines))
            put (st { _editor = ed, _selAnchor = Nothing, _mouseDragging = False })

-- | Move the cursor up (negative) or down by n lines, keeping its column
--   where possible. Used for the mouse wheel and scroll bar.
moveCursorLines :: Int -> EventM Name AppState ()
moveCursorLines n = do
    st <- get
    let (r, c) = E.getCursorPosition (_editor st)
    put (st { _editor = E.applyEdit (Z.moveCursorClosest (r + n, c)) (_editor st) })

-- | Run an editor mutation; if it changed the text, mark the buffer dirty,
--   record an undo snapshot (see the heuristic above), and schedule the
--   debounced recompile.
trackEdit :: BChan CustomEvent -> EventM Name AppState () -> EventM Name AppState ()
trackEdit chan action = do
    oldText <- E.getEditContents . _editor <$> get
    action
    st' <- get
    let newText = E.getEditContents (_editor st')
    when (oldText /= newText) $ do
        -- Increment the version, set dirty flag, and fork the timer thread
        let nextVersion = _editVersion st' + 1
            newUndoStack
                | _pendingUndoSnapshot st' = unlines oldText : _undoStack st'
                | otherwise                 = _undoStack st'
        put (st' { _isDirty = True
                 , _editVersion = nextVersion
                 , _undoStack = newUndoStack
                 , _redoStack = []
                 , _pendingUndoSnapshot = False
                 })
        scheduleCompile chan nextVersion

-- | Start the two debounce timers for edit `version`: the recompile fires
--   after the configured `auto_compile_delay_ms` (short, since number-only
--   edits now show in the viewer without a shader compile), and the undo
--   grouping boundary after a fixed 500ms pause. Each only acts if no newer
--   edit has happened by the time it fires.
scheduleCompile :: BChan CustomEvent -> Int -> EventM Name AppState ()
scheduleCompile chan version = do
    delayMs <- max 0 . cfgAutoCompileDelayMs . _config <$> get
    _ <- liftIO $ forkIO $ do
        threadDelay (delayMs * 1000)
        writeBChan chan (CompileTimerFired version)
    _ <- liftIO $ forkIO $ do
        threadDelay (undoGroupingMs * 1000)
        writeBChan chan (UndoBoundary version)
    return ()

-- | Edits separated by less than this pause collapse into one undo step.
undoGroupingMs :: Int
undoGroupingMs = 500

-- | Best-effort copy to the OS clipboard so text can be pasted into other
--   programs. Runs in the background; failures (no clipboard tool) are
--   ignored, the internal clipboard still works.
copyToSystemClipboard :: String -> IO ()
copyToSystemClipboard txt = void $ forkIO $ void $ try' $
    case os of
        "mingw32" -> pipeTo "clip" []
        "darwin"  -> pipeTo "pbcopy" []
        _         -> pipeTo "wl-copy" [] `orElse` pipeTo "xclip" ["-selection", "clipboard"]
  where
    -- clip.exe expects CRLF line endings.
    payload = if os == "mingw32" then concatMap (\c -> if c == '\n' then "\r\n" else [c]) txt else txt
    pipeTo cmd args = do
        (code, _, _) <- readProcessWithExitCode cmd args payload
        return (code == ExitSuccess)
    orElse a b = do
        ok <- try' a
        if ok == Just True then return True else b
    try' :: IO a -> IO (Maybe a)
    try' act = either (\e -> const Nothing (e :: SomeException)) Just <$> try act


-- | 3. UNSAVED CHANGES PROMPT
handleUnsavedPrompt :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleUnsavedPrompt _ (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    put (st { _mode = Editing })

handleUnsavedPrompt _ (VtyEvent (V.EvKey (V.KChar 'n') [])) = do
    st <- get
    put (st { _mode = Splash, _isDirty = False })
handleUnsavedPrompt chan (VtyEvent (V.EvKey (V.KChar 'N') [])) = handleUnsavedPrompt chan (VtyEvent (V.EvKey (V.KChar 'n') []))

handleUnsavedPrompt _ (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    case _currentFile st of
        Just path -> do
            let code = unlines $ E.getEditContents (_editor st)
            (_, newRecents) <- liftIO $ writeAndCompile st path code
            put (st { _mode = Splash, _isDirty = False, _recentFiles = newRecents })
        Nothing ->
            put (st { _mode = SaveDialog })
handleUnsavedPrompt _ _ = return ()


-- | 4. SAVE DIALOG
handleSaveDialog :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleSaveDialog _ (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    put (st { _mode = Editing })

handleSaveDialog _ (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    let filenameLines = E.getEditContents (_saveInput st)
        filename = if null filenameLines || null (head filenameLines) 
                   then "untitled.irsm" 
                   else head filenameLines
        code = unlines $ E.getEditContents (_editor st)

    absPath <- liftIO $ makeAbsolute filename
    (newStatus, newRecents) <- liftIO $ writeAndCompile st absPath code

    put (st { _mode = Editing
            , _currentFile = Just absPath
            , _saveInput = E.editor SaveEditor (Just 1) ""
            , _status = newStatus
            , _recentFiles = newRecents
            , _isDirty = False
            })

handleSaveDialog _ ev = do
    zoom saveInputLens $ E.handleEditorEvent ev


-- | 5. OPEN DIALOG
handleOpenDialog :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleOpenDialog _ (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    let nextMode = if _currentFile st == Nothing && E.getEditContents (_editor st) == [""] 
                   then Splash else Editing
    put (st { _mode = nextMode, _status = Normal })

handleOpenDialog _ (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    let pathLines = E.getEditContents (_openInput st)
        path = if null pathLines || null (head pathLines) then "" else head pathLines
    
    exists <- liftIO $ doesFileExist path
    if exists
        then openFile st path
        else put (st { _status = ErrorMsg "File not found!" 0 })

handleOpenDialog _ ev = do
    zoom openInputLens $ E.handleEditorEvent ev

-- | 6. CONFIG EDITOR
handleConfigEditing :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()

-- ESC: discard changes, go back
handleConfigEditing _ (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    let prevMode = if _currentFile st == Nothing && E.getEditContents (_editor st) == [""]
                   then Splash else Editing
    put (st { _mode = prevMode })

-- Ctrl+S: save the config file, reload config, send to viewer
handleConfigEditing _ (VtyEvent (V.EvKey (V.KChar 's') [V.MCtrl])) = do
    st <- get
    let content = unlines $ E.getEditContents (_configInput st)
    configPath <- liftIO getConfigPath
    liftIO $ writeFile configPath content
    newConfig <- liftIO $ loadConfig configPath
    liftIO $ sendConfig newConfig
    let prevMode = if _currentFile st == Nothing && E.getEditContents (_editor st) == [""]
                   then Splash else Editing
    put (st { _mode = prevMode, _config = newConfig, _status = Saved
            , _currentFilter = cfgFilter newConfig })

-- Everything else: pass through to the editor widget
handleConfigEditing _ ev = do
    zoom configInputLens $ E.handleEditorEvent ev

-- | 7. COMMAND PALETTE
handleCommandPalette :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleCommandPalette _ (VtyEvent (V.EvKey V.KEsc [])) = do
    st <- get
    put (st { _mode = Editing })

handleCommandPalette _ (VtyEvent (V.EvKey V.KUp [])) = do
    st <- get
    let count = length (filteredCommands st)
    when (count > 0) $
        put (st { _paletteSelected = (_paletteSelected st - 1) `mod` count })

handleCommandPalette _ (VtyEvent (V.EvKey V.KDown [])) = do
    st <- get
    let count = length (filteredCommands st)
    when (count > 0) $
        put (st { _paletteSelected = (_paletteSelected st + 1) `mod` count })

handleCommandPalette chan (VtyEvent (V.EvKey V.KEnter [])) = do
    st <- get
    case drop (_paletteSelected st) (filteredCommands st) of
        (cmdId:_) -> do
            put (st { _mode = Editing })
            runCommand chan cmdId
        [] -> put (st { _mode = Editing })

-- Everything else: pass through to the filter input, and reset the
-- selection since the filtered list may have changed.
handleCommandPalette _ ev = do
    zoom paletteInputLens $ E.handleEditorEvent ev
    st <- get
    put (st { _paletteSelected = 0 })