module AppState where

import qualified Brick.Widgets.Edit as E
import Lens.Micro (Lens')
import System.Process (ProcessHandle)
import Data.Char (toLower, toUpper)
import Data.List (isInfixOf, intercalate)
import Data.Maybe (fromMaybe)
import Config (IronConfig, defaultConfig, cfgKeybindings, defaultKeybindings)

data CustomEvent = CompileTimerFired Int
    deriving (Show)

data AppMode = Splash | Editing | SaveDialog | OpenDialog | UnsavedPrompt | ConfigEditing | CommandPalette
    deriving (Eq)

data Name = CodeEditor | SaveEditor | OpenEditor | ConfigEditor | PaletteEditor
    deriving (Eq, Ord, Show)

data AppStatus = Normal | Saved | Exported FilePath | ErrorMsg String Int

data ViewerMode = OrbitMode | StaticMode | FlyMode
    deriving (Eq, Show)

data AppState = AppState
    { _mode         :: AppMode
    , _viewerMode   :: ViewerMode
    , _editor       :: E.Editor String Name
    , _saveInput    :: E.Editor String Name 
    , _openInput    :: E.Editor String Name 
    , _configInput  :: E.Editor String Name
    , _paletteInput :: E.Editor String Name
    , _paletteSelected :: Int
    , _currentFile  :: Maybe FilePath
    , _recentFiles  :: [FilePath]
    , _status       :: AppStatus
    , _isDirty      :: Bool
    , _viewerHandle :: Maybe ProcessHandle
    , _editVersion  :: Int
    , _config       :: IronConfig
    }

editorLens :: Lens' AppState (E.Editor String Name)
editorLens f st = (\e -> st { _editor = e }) <$> f (_editor st)

saveInputLens :: Lens' AppState (E.Editor String Name)
saveInputLens f st = (\e -> st { _saveInput = e }) <$> f (_saveInput st)

openInputLens :: Lens' AppState (E.Editor String Name)
openInputLens f st = (\e -> st { _openInput = e }) <$> f (_openInput st)

configInputLens :: Lens' AppState (E.Editor String Name)
configInputLens f st = (\e -> st { _configInput = e }) <$> f (_configInput st)

paletteInputLens :: Lens' AppState (E.Editor String Name)
paletteInputLens f st = (\e -> st { _paletteInput = e }) <$> f (_paletteInput st)

-- ─── COMMAND REGISTRY ────────────────────────────────────────────────────────
-- Shared between AppEvents (keybinding dispatch) and AppUi (palette rendering).

-- | Commands available via keybindings and the command palette.
data CommandId = CmdCommandPalette | CmdSave | CmdOpenFile | CmdSettings | CmdCycleViewMode | CmdResetCamera | CmdExportOBJ
    deriving (Eq, Enum, Bounded, Show)

-- | The id used to look up/override this command's keybinding in the config file.
commandConfigKey :: CommandId -> String
commandConfigKey CmdCommandPalette = "command_palette"
commandConfigKey CmdSave           = "save"
commandConfigKey CmdOpenFile       = "open_file"
commandConfigKey CmdSettings       = "settings"
commandConfigKey CmdCycleViewMode  = "cycle_view_mode"
commandConfigKey CmdResetCamera    = "reset_camera"
commandConfigKey CmdExportOBJ      = "export_obj"

-- | Human-readable label shown in the command palette.
commandLabel :: CommandId -> String
commandLabel CmdCommandPalette = "Open Command Palette"
commandLabel CmdSave           = "Save File"
commandLabel CmdOpenFile        = "Open File..."
commandLabel CmdSettings        = "Open Settings"
commandLabel CmdCycleViewMode   = "Cycle Viewer Mode"
commandLabel CmdResetCamera     = "Reset Camera"
commandLabel CmdExportOBJ        = "Export to .OBJ"

-- | The configured (or default) key combo string for a command, e.g. "ctrl+s".
keybindingFor :: IronConfig -> CommandId -> String
keybindingFor cfg cmdId =
    let cid = commandConfigKey cmdId
    in fromMaybe (fromMaybe "" (lookup cid defaultKeybindings)) (lookup cid (cfgKeybindings cfg))

-- | Split a key-combo string like "ctrl+shift+p" into its parts.
splitOnPlus :: String -> [String]
splitOnPlus s = case break (== '+') s of
    (w, '+':rest) -> w : splitOnPlus rest
    (w, _)        -> [w]

-- | Format a key combo for display, e.g. "ctrl+s" -> "Ctrl+S".
formatKeyCombo :: String -> String
formatKeyCombo = intercalate "+" . map capitalize . splitOnPlus
  where
    capitalize []     = []
    capitalize (c:cs) = toUpper c : cs

-- | Commands whose label matches the palette's current filter text
--   (case-insensitive substring match).
filteredCommands :: AppState -> [CommandId]
filteredCommands st =
    let query = map toLower (concat (E.getEditContents (_paletteInput st)))
    in [ cmdId | cmdId <- [minBound .. maxBound]
               , query `isInfixOf` map toLower (commandLabel cmdId) ]