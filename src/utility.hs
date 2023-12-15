-- utility.hs
module Utility where

import Data
import qualified Data.Map as M
import Control.Monad (void)
import Data.Maybe (fromMaybe, catMaybes)

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
import System.Directory (doesFileExist)
import System.IO (writeFile)










-- this function takes into an appstate and generate a id to index map, 
-- the map takes a id of maintask and return the rank of the maintask in corresponding list.
-- Helper function to process a single L.List Name Task and extract main tasks with their indexes
processList :: L.List Name Task -> M.Map Int Int
processList taskList = M.fromList $ catMaybes $ zipWith extractTaskId [1..] (Vec.toList $ Vec.filter (not . isSub) $ L.listElements taskList)
  where
    extractTaskId idx (IMT (id, _)) = Just (id, idx)
    extractTaskId idx (UT  (id, _)) = Just (id, idx)
    extractTaskId idx (MUT (id, _)) = Just (id, idx)
    extractTaskId idx (NNT (id, _)) = Just (id, idx)
    _       = Nothing  -- Ignore SUB tasks

-- The main function to generate id to index map
genIdToRankM :: AppState -> M.Map Int Int
genIdToRankM appState =
  M.unions [
    processList (imList appState),
    processList (uList appState),
    processList (muList appState),
    processList (nnList appState),
    processList (donelist appState)
  ]

-- Example usage
-- let idToRankMap = genIdToRankM initialState
-- M.lookup 0 idToRankMap -- For example, to find the rank of task with ID 0

-- replace the tasks at the appointed positon of the given list using the given task
replaceTask :: Int -> Task -> L.List Name Task -> L.List Name Task
replaceTask idx modifiedT l = L.listMoveBy (1) $ L.listInsert (idx) modifiedT $ L.listRemove idx l

changeTaskContent :: Task -> String -> Task 
changeTaskContent = go
    where 
        go (IMT (n, s)) s'    = (IMT (n, s'))   
        go (UT  (n, s)) s'    = (UT  (n, s'))  
        go (MUT (n, s)) s'    = (MUT (n, s'))  
        go (NNT (n, s)) s'    = (NNT (n, s'))  
        go (SUB (n, b, s)) s' = (SUB (n, b, s'))  


-- this function takes into a task and return a string
getContent :: Task -> String 
getContent = go 
    where 
        go    (IMT (_, s)) = s
        go    (UT  (_, s)) = s
        go    (MUT (_, s)) = s
        go    (NNT (_, s)) = s
        go    (SUB (_, _, s)) = s 

-- This function will takes into a task and return its priority
getPriority :: Task -> Int
getPriority (MUT _) = 1
getPriority (UT  _) = 2
getPriority (IMT _) = 3
getPriority (NNT _) = 4
getPriority  _      = 0


-- this function takes a list of tasks and a id then return the length of the current task
getTaskLen :: L.List Name Task -> Int -> Int
getTaskLen l idx = go (Vec.toList (L.listElements l )) idx 0
    where 
        go (h:t) id count = if getTaskId h == idx then go t idx (count+1)
                                                   else go t idx count
        go _     _  count = count

-- this function takes into a list and a id then return the index +1 of the last subtask, If no such task, then output the length of the list
getTaskEndIndex :: L.List Name Task -> Int -> Int
getTaskEndIndex l idx = go (Vec.toList (L.listElements l )) idx 0 False
    where 
        go (h:t) id count False = if getTaskId h == idx then go t idx (count+1) True
                                                   else go t idx (count+1) False
        go (h:t) id count True  = if getTaskId h == idx then go t idx (count+1) True
                                                   else go t idx (count) True

        go _     _  count _     = count 


-- This function takes a list of tasks and insert it in to the head of another list
insertGL ::  [Task] -> L.List Name Task -> L.List Name Task
insertGL v l = go (reverse v) l
    where 
        go (h:t) updatel = go t (L.listInsert 0 h updatel)
        go _     updatel = updatel


-- THis function takes the input string and appstate and return a updated appstate
setInputField :: Maybe String -> AppState -> AppState
setInputField s appState = appState { inputField = s }

--  THis function takes the pointer (Int) and return the corresponding list
getList :: Int -> AppState -> L.List Name Task
getList 1 s = muList   s 
getList 2 s = uList    s 
getList 3 s = imList   s 
getList 4 s = nnList   s 
getList 5 s = donelist s 


--  THis function takes index and list and update the corresponding list in appstate
insertState :: Int -> L.List Name Task -> AppState -> AppState
insertState 1 l s =  s {muList   = l}
insertState 2 l s =  s {uList    = l}
insertState 3 l s =  s {imList   = l}
insertState 4 l s =  s {nnList   = l}
insertState 5 l s =  s {donelist = l}
insertState _ _ s =  s 

-- Input: index, maxId, text, and output Task
createMainTask :: Int -> Int -> String -> Task
createMainTask = go
    where 
        go 1 maxId s = MUT (maxId,s)
        go 2 maxId s = UT  (maxId,s)
        go 3 maxId s = IMT (maxId,s)
        go 4 maxId s = NNT (maxId,s)
        go _ maxId s = MUT (maxId,s)

createSubTask :: Int -> String -> Task
createSubTask idx s = SUB (idx, False, s)



isSub :: Task -> Bool -- tell whether a task is a subtask
isSub (SUB _) = True
isSub _       = False


setTaskDone :: Task -> Task 
setTaskDone = go 
    where go (SUB (n,_,s)) = SUB (n,True,s)
          go  other        = other

setTaskToDo :: Task -> Task 
setTaskToDo = go 
    where go (SUB (n,_,s)) = SUB (n,False,s)
          go  other        = other

getTaskId :: Task -> Int
getTaskId = go 
    where 
        go (IMT (id, _)) = id
        go (UT  (id, _)) = id
        go (MUT (id, _)) = id
        go (NNT (id, _)) = id
        go (SUB (id, _, _)) = id

-- this function takes into a list of task and a id, then return the deleted tasks (correct order) and the updated l (correct order)
getDelsTandNewL :: L.List Name Task -> Int -> ([Task], L.List Name Task)
getDelsTandNewL l id = let v = L.listElements l in go (Vec.toList v) id [] []
    where 
        go (h:t) id xs os =
            if curId == id
                then go t id (setTaskDone h:xs) os
                else go t id xs (h:os)
            where
                curId = getTaskId h

        go [] _ xs os = (reverse xs, L.listReplace (Vec.fromList (reverse os)) (Just 0) l)
                        

getLen :: L.List Name Task -> Int
getLen = length

--  given pointer index,  state  return the maxId in the list that index pointed to
getMaxId ::  AppState -> Int
getMaxId  s = curMaxId s



-- given index, appstate, and new maxId   return a updated appstate
-- here the reason why we decrease the index by 1 is because the gap between the index of 5 task list and index of general list
setMaxId :: AppState -> Int -> AppState
setMaxId  s newMaxId = s { curMaxId = newMaxId}


exportState :: AppState -> FilePath -> IO ()
exportState appState filePath = 
    do    
        checkAndCreateFile filePath
        B.writeFile filePath (encode appState)

importState :: FilePath -> IO (Maybe AppState)
importState filePath = do
    checkAndCreateFile filePath
    file <- B.readFile filePath
    return $ decode file


checkAndCreateFile :: FilePath -> IO ()
checkAndCreateFile filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then return ()
        else do
            writeFile filePath ""
            return ()