module AppState where

import qualified Brick.Widgets.Edit as E
import Lens.Micro (Lens')
import System.Process (ProcessHandle)

data AppMode = Splash | Editing | SaveDialog | OpenDialog | UnsavedPrompt
    deriving (Eq)

data Name = CodeEditor | SaveEditor | OpenEditor
    deriving (Eq, Ord, Show)

-- FIX: ErrorMsg now holds (Error String) and (Line Number)
data AppStatus = Normal | Saved | ErrorMsg String Int 

data AppState = AppState
    { _mode        :: AppMode
    , _editor      :: E.Editor String Name
    , _saveInput   :: E.Editor String Name 
    , _openInput   :: E.Editor String Name 
    , _currentFile :: Maybe FilePath     
    , _recentFiles :: [FilePath]           
    , _status      :: AppStatus
    , _isDirty     :: Bool
    , _viewerHandle :: Maybe ProcessHandle 
    }

editorLens :: Lens' AppState (E.Editor String Name)
editorLens f st = (\e -> st { _editor = e }) <$> f (_editor st)

saveInputLens :: Lens' AppState (E.Editor String Name)
saveInputLens f st = (\e -> st { _saveInput = e }) <$> f (_saveInput st)

openInputLens :: Lens' AppState (E.Editor String Name)
openInputLens f st = (\e -> st { _openInput = e }) <$> f (_openInput st)