module AppState where

import qualified Brick.Widgets.Edit as E
import Lens.Micro (Lens')

data AppMode = Splash | Editing | SaveDialog | OpenDialog
    deriving (Eq)

data Name = CodeEditor | SaveEditor | OpenEditor
    deriving (Eq, Ord, Show)

data AppStatus = Normal | Saved | ErrorMsg String

data AppState = AppState
    { _mode        :: AppMode
    , _editor      :: E.Editor String Name
    , _saveInput   :: E.Editor String Name 
    , _openInput   :: E.Editor String Name 
    , _currentFile :: Maybe FilePath     
    , _recentFiles :: [FilePath]           
    , _status      :: AppStatus
    }

editorLens :: Lens' AppState (E.Editor String Name)
editorLens f st = (\e -> st { _editor = e }) <$> f (_editor st)

saveInputLens :: Lens' AppState (E.Editor String Name)
saveInputLens f st = (\e -> st { _saveInput = e }) <$> f (_saveInput st)

openInputLens :: Lens' AppState (E.Editor String Name)
openInputLens f st = (\e -> st { _openInput = e }) <$> f (_openInput st)