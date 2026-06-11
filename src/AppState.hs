module AppState where

import qualified Brick.Widgets.Edit as E
import Lens.Micro (Lens')
import System.Process (ProcessHandle)
import Config (IronConfig, defaultConfig)

data CustomEvent = CompileTimerFired Int
    deriving (Show)

data AppMode = Splash | Editing | SaveDialog | OpenDialog | UnsavedPrompt | ConfigEditing
    deriving (Eq)

data Name = CodeEditor | SaveEditor | OpenEditor | ConfigEditor
    deriving (Eq, Ord, Show)

data AppStatus = Normal | Saved | ErrorMsg String Int 

data ViewerMode = OrbitMode | StaticMode | FlyMode
    deriving (Eq, Show)

data AppState = AppState
    { _mode         :: AppMode
    , _viewerMode   :: ViewerMode
    , _editor       :: E.Editor String Name
    , _saveInput    :: E.Editor String Name 
    , _openInput    :: E.Editor String Name 
    , _configInput  :: E.Editor String Name
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