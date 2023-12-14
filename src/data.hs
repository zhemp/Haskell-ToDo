-- data.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Data where

import Control.Monad (void)
import Data.Maybe (fromMaybe, catMaybes)
-- #if !(MIN_VERSION_base(4,11,0))
-- import Data.Monoid
-- #endif
import qualified Graphics.Vty as V
import Lens.Micro ((^.))

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, vBox, vLimit, withAttr, (<+>), (<=>),hBox, fill, emptyWidget)
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Data.Foldable as Vector
import qualified Data.IMap as Vector
import qualified Data.Foldable as V
import qualified Data.Map as M
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy (pack, unpack)
import Control.Monad.IO.Class (liftIO)

-- (the id of the current task, the content)
type L1Task = (Int, String) --()


-- (SUB  (represent the id of its main task,  Bool shows whether the subtask has been done: True = done, False = todo, String is whether it has been done))
data Task = 
   IMT L1Task
 | UT  L1Task
 | MUT L1Task
 | NNT L1Task
 | SUB (Int, Bool, String) 
 deriving (Show)

data Name = Imp | Urg | Impurg | Nn | Done -- Add more names as needed
  deriving (Eq, Ord, Show)

data AppState = AppState {
    pointer   :: Int,
    -- 0 is adding main task, 1 is for adding sub task， 2 is for modifying task
    status    :: Int,
    theme     :: Int,
    imList    :: L.List Name Task,
    uList     :: L.List Name Task, 
    muList    :: L.List Name Task, 
    nnList    :: L.List Name Task,
    donelist  :: L.List Name Task,
    curMaxId  :: Int,
    inputField :: Maybe String,  --  used to store the input string
    errorMessage :: Maybe String
} deriving (Generic, Show)


initialState :: AppState

initialState = AppState {
    pointer  = 1,
    status   = 0,
    theme    = 0,
    imList   = L.list Imp    (Vec.fromList [(IMT (0, "test")), (SUB (0, True, "line")), (IMT (1, "test")), (IMT (2, "test"))]) 0,
    uList    = L.list Urg    (Vec.fromList [(UT (3, "test")), (SUB (3, True, "line")), (UT (4, "test")), (UT (5, "test"))]) 0,
    muList   = L.list Impurg (Vec.fromList [(MUT (6, "test")), (SUB (6, False, "line1")),(SUB (6, False, "line2")), (MUT (7, "test")), (MUT (8, "test"))]) 0,
    nnList   = L.list Nn     (Vec.fromList [(NNT (9, "test")), (SUB (9, True, "line")), (NNT (10, "test")), (NNT (11, "test"))]) 0,
    donelist = L.list Done   (Vec.empty) 0,
    curMaxId = 11,
    inputField = Nothing,
    errorMessage = Nothing
        }

emptyS = AppState {
      pointer  = 1,
      status   = 0,
      theme    = 0,
      imList   = L.list Imp    (Vec.empty) 0,
      uList    = L.list Urg    (Vec.empty) 0,
      muList   = L.list Impurg (Vec.empty) 0,
      nnList   = L.list Nn     (Vec.empty) 0,
      donelist = L.list Done   (Vec.empty) 0,
      curMaxId = 0,
      inputField = Nothing,       
      errorMessage = Nothing
  }


instance ToJSON Task where
    toJSON (IMT (id, content)) = object ["type" .= ("IMT" :: String), "id" .= id, "content" .= content]
    toJSON (UT (id, content)) = object ["type" .= ("UT" :: String), "id" .= id, "content" .= content]
    toJSON (MUT (id, content)) = object ["type" .= ("MUT" :: String), "id" .= id, "content" .= content]
    toJSON (NNT (id, content)) = object ["type" .= ("NNT" :: String), "id" .= id, "content" .= content] 
    toJSON (SUB (id, done, content)) = object ["type" .= ("SUB" :: String), "id" .= id, "done" .= done, "content" .= content]  

instance FromJSON Task where
    parseJSON = withObject "Task" $ \o -> do
        t <- o .: "type"
        case t :: String of
            "IMT" -> IMT <$> ((,) <$> o .: "id" <*> o .: "content")
            "UT" -> UT <$> ((,) <$> o .: "id" <*> o .: "content")
            "MUT" -> MUT <$> ((,) <$> o .: "id" <*> o .: "content")
            "NNT" -> NNT <$> ((,) <$> o .: "id" <*> o .: "content")
            "SUB" -> SUB <$> ((,,) <$> o .: "id" <*> o .: "done" <*> o .: "content")
            _ -> fail "Unknown type"

instance ToJSON AppState where
    toJSON  appState =object [
        "pointer" .= pointer appState,
        "status" .= status appState,
        "theme" .= theme appState,
        "imList" .= L.listElements (imList appState),
        "uList" .= L.listElements (uList appState),
        "muList" .= L.listElements (muList appState),
        "nnList" .= L.listElements (nnList appState),
        "donelist" .= L.listElements (donelist appState),
        "curMaxId" .= curMaxId appState,
        "inputField" .= inputField appState,
        "errorMessage" .= errorMessage appState
        ]

instance FromJSON AppState where
    parseJSON = withObject "AppState" $ \o -> do
        pointer <- o .: "pointer"
        status <- o .: "status"
        imList <- L.list Imp <$> o .: "imList" <*> pure 1
        uList <- L.list Urg <$> o .: "uList" <*> pure 1
        muList <- L.list Impurg <$> o .: "muList" <*> pure 1
        nnList <- L.list Nn <$> o .: "nnList" <*> pure 1
        donelist <- L.list Done <$> o .: "donelist" <*> pure 1
        curMaxId <- o .: "curMaxId"
        inputField <- o .: "inputField"
        errorMessage <- o .: "errorMessage"
        theme <- o .: "theme"
        return AppState {
            pointer = pointer,
            status = status,
            theme = theme,
            imList = imList,
            uList = uList,
            muList = muList,
            nnList = nnList,
            donelist = donelist,
            curMaxId = curMaxId,
            inputField = inputField,
            errorMessage = errorMessage
        }

exportState :: AppState -> FilePath -> IO ()
exportState appState filePath =     
    B.writeFile filePath (encode appState)

importState :: FilePath -> IO (Maybe AppState)
importState filePath = do
    file <- B.readFile filePath
    return $ decode file