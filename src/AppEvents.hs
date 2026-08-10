module AppEvents where

import Brick
import Brick.BChan (BChan, writeBChan)
import Brick.Types (zoom)
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Control.Concurrent (forkIO, threadDelay)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory, makeAbsolute)
import System.FilePath (replaceExtension, takeExtension, dropExtension)
import Data.Char (isDigit, toLower)
import Data.List (sort, elemIndex)
import Data.Maybe (listToMaybe, fromMaybe)

import AppState
import Config (loadConfig, defaultConfigText, readFileStrict, cfgDefaultObjectColor, cfgExportResolution, cfgFilter, cfgVideoFps, IronConfig)
import AppCore

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
        -- The 500ms quiet period has elapsed with no further typing, so the
        -- *next* edit starts a brand-new undo boundary (see handleEditorInput).
        put (st { _status = newStatus, _pendingUndoSnapshot = True })

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
            , _undoStack = [], _redoStack = [], _pendingUndoSnapshot = True })
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

runCommand chan CmdExportOBJ = do
    st <- get
    case _currentFile st of
        Nothing -> put (st { _status = ErrorMsg "Save the file before exporting" 0 })
        Just _ | _exportProgress st /= Nothing -> return () -- export already in progress
        Just path -> do
            let code = unlines $ E.getEditContents (_editor st)
                objPath = replaceExtension path "obj"
                resolution = cfgExportResolution (_config st)
                color = cfgDefaultObjectColor (_config st)
            put (st { _exportProgress = Just 0.0 })
            _ <- liftIO $ forkIO $ do
                result <- exportModelToOBJ color code objPath resolution
                                          (\p -> writeBChan chan (ExportProgress p))
                writeBChan chan (ExportFinished result)
            return ()

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
                    , _isDirty = True
                    , _editVersion = nextVersion
                    })
            liftIO $ forkIO $ do
                threadDelay 500000
                writeBChan chan (CompileTimerFired nextVersion)
            return ()

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
                    , _isDirty = True
                    , _editVersion = nextVersion
                    })
            liftIO $ forkIO $ do
                threadDelay 500000
                writeBChan chan (CompileTimerFired nextVersion)
            return ()

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
-- at a time). Instead we coalesce edits into the same 500ms "debounce
-- window" already used for auto-compile: a snapshot of the *pre-edit* text
-- is only pushed when `_pendingUndoSnapshot` is True, which happens (a) on
-- the very first edit after loading/opening a file, and (b) whenever the
-- debounce timer actually fires (see CompileTimerFired above), meaning the
-- user paused for >=500ms. Any burst of typing/deleting within one window
-- collapses to a single undo step; pausing (or the compile firing) starts a
-- fresh boundary for the next burst. A new edit after an undo always clears
-- the redo stack (standard undo/redo semantics).
handleEditorInput :: BChan CustomEvent -> BrickEvent Name CustomEvent -> EventM Name AppState ()
handleEditorInput chan ev = do
    st <- get
    let oldText = E.getEditContents (_editor st)

    zoom editorLens $ E.handleEditorEvent ev

    st' <- get
    let newText = E.getEditContents (_editor st')

    if oldText /= newText
        then do
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

            liftIO $ forkIO $ do
                threadDelay 500000 -- Sleep for 500ms
                writeBChan chan (CompileTimerFired nextVersion)
            return ()
        else
            put st'


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