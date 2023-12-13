{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

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
import Text.Read (Lexeme(String))





drawUI ::  AppState -> [Widget Name]
drawUI appState = [ui]
    where
        index_map = genIdToRankM appState
        errmsg = errorMessage appState
        focus = pointer appState --get the current focused list id

        total_mu = Vec.length $ Vec.filter (not . isSub) (L.listElements (muList appState))
        total_mu_sub = Vec.length $ Vec.filter isSub (L.listElements (muList appState))
        total_u = Vec.length $ Vec.filter (not . isSub) (L.listElements (uList appState)) 
        total_u_sub = Vec.length $ Vec.filter isSub (L.listElements (uList appState)) 
        total_m = Vec.length $ Vec.filter (not . isSub) (L.listElements (imList appState)) 
        total_m_sub = Vec.length $ Vec.filter isSub (L.listElements (imList appState)) 
        total_nn = Vec.length $ Vec.filter (not . isSub) (L.listElements (nnList appState)) 
        total_nn_sub = Vec.length $ Vec.filter isSub (L.listElements (nnList appState)) 
        undone_total = str $ show (total_mu + total_u + total_m + total_nn)
        total_done = Vec.length $ Vec.filter (not . isSub) (L.listElements (donelist appState))   --get the current count of tasks

        mubox = B.borderWithLabel (str ("Imp and Urgent: " ++ show total_mu ++ " main tasks " ++ show total_mu_sub ++ " sub-tasks")) $
                L.renderList (listDrawElement index_map) (focus == 1) (muList appState)
        ubox = B.borderWithLabel (str ("Urgent: " ++ show total_u ++ " main tasks " ++ show total_u_sub ++ " sub-tasks")) $
                L.renderList (listDrawElement index_map) (focus == 2) (uList appState)
        mbox = B.borderWithLabel (str ("Imp: " ++ show total_m ++ " main tasks " ++ show total_m_sub ++ " sub-tasks")) $
                L.renderList (listDrawElement index_map) (focus == 3) (imList appState)
        nnbox = B.borderWithLabel (str ("Not imp nor urgent: " ++ show total_nn ++ " main tasks " ++ show total_nn_sub ++ " sub-tasks")) $
                L.renderList (listDrawElement index_map) (focus == 4) (nnList appState)
        doneBox = B.borderWithLabel (str ("Done: " ++ show total_done ++ " tasks")) $ 
                L.renderList (listDrawElement index_map) (focus == 5) (donelist appState)

        ui = case inputField appState of
            Nothing -> C.hCenter $ C.vCenter $ hLimit 130 $ vLimit 50 $ B.borderWithLabel (str "Fantastic To-do") $ 
                C.vCenter $ vBox [ C.hCenter (str "You have a total of " <+> undone_total <+> str " tasks undone and " <+> str (show total_done) <+> str " done"),
                                B.hBorder,
                                hBox [vBox [mubox,
                                            ubox,
                                            mbox,
                                            nnbox
                                            ],
                                        doneBox
                                    ],
                                B.hBorder,
                                vLimit 3 $ vBox [
                                    hBox[C.center (str "Add Main Task"), B.vBorder, C.center (str "Add Sub Task"), B.vBorder, C.center (str "Delete"), B.vBorder, C.center (str "Mark as Done"), B.vBorder, C.center (str "Mark as Undone") ],
                                    B.hBorder,
                                    hBox[C.center (str "1"), B.vBorder, C.center (str "2"), B.vBorder, C.center (str "-"), B.vBorder, C.center (str "4"), B.vBorder, C.center (str "5")]
                                ],
                                case errmsg of
                                    Just err -> vLimit 3 $ vBox[
                                                        C.center $ str err
                                                        , B.hBorder
                                                        , C.center $ str "Press any key to ignore this error."
                                                    ]
                                    Nothing -> emptyWidget
                                ]
            Just input -> C.hCenter $ C.vCenter $ hLimit 131 $ vLimit 50 $ B.borderWithLabel (str "Fantastic To-do") $ 
                C.vCenter $ vBox [ C.hCenter (str "You have a total of " <+> undone_total <+> str " tasks undone and " <+> str (show total_done) <+> str " done"),
                                B.hBorder,
                                hBox [vBox [mubox,
                                            ubox,
                                            mbox,
                                            nnbox
                                            ],
                                        doneBox
                                    ],
                                B.hBorder,
                                vLimit 3 $ vBox [
                                    hBox[C.center (str "----"), B.vBorder, C.center (str "----"), B.vBorder, C.center (str "----"), B.vBorder, C.center (str "Enter")],
                                    B.hBorder,
                                    hBox[C.center (str "+"), B.vBorder, C.center (str "+"), B.vBorder, C.center (str "+"), B.vBorder, C.center (str "Finish")]
                                ]
                                , case input of 
                                    "" -> str "Please input your task content!"
                                    _ -> str input
                                ,
                                case errmsg of
                                    Just err -> vLimit 3 $ vBox[
                                                        C.center $ str err
                                                        , B.hBorder
                                                        , C.center $ str "Press any key to ignore this error."
                                                    ]
                                    Nothing -> emptyWidget
                                ]
            

-- listDrawElement :: M.Map Int Int -> Bool -> Task -> Widget Name
-- listDrawElement map _ task =
--     case task of 
--         SUB (_, _, _) -> str "  └── " <+> str (show task)
--         _ -> str (show task)

listDrawElement :: M.Map Int Int -> Bool -> Task -> Widget Name --replace he current draw with this
listDrawElement map _ task =
        case task of
        SUB (_, done, content) -> if done then str "  └── " <+> str (concatMap (\c -> [c, '\x0336']) content)
                                            else str "  └── " <+> str content 
        IMT (id, content) -> let Just index = (M.lookup id map) 
                            in str (show index) <+> str "." <+> str content
        UT  (id, content) -> let Just index = (M.lookup id map) 
                            in str (show index) <+> str "." <+> str content
        MUT (id, content) -> let Just index = (M.lookup id map) 
                            in str (show index) <+> str "." <+> str content
        NNT (id, content) -> let Just index = (M.lookup id map) 
                            in str (show index) <+> str "." <+> str content



appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next (AppState))
appEvent as (T.VtyEvent (V.EvKey (V.KChar 'm') [])) = if status as == 5 then M.continue $ emptyS
                                                                        else M.continue $ as
appEvent appState (T.VtyEvent e) = 
    let noErrApST = appState {errorMessage = Nothing,status =0}
    in
    case inputField noErrApST of
        --  if the inputField is not empty, then we will handle the input
        Just input -> 
             case e of
                V.EvKey V.KEnter [] -> 
                    let 
                        curStatus = status noErrApST 
                    in 
                        case L.listSelectedElement l of
                            Just (pos, task) ->
                                    if curStatus == 0 then 
                                        let maxId = getMaxId noErrApST
                                            el = createMainTask index (maxId+1) input
                                            newPos = getTaskEndIndex l (getTaskId task)
                                        in 
                                            M.continue $ setInputField Nothing $ insertState index (L.listMoveTo newPos $ L.listInsert newPos el l) $ setMaxId  noErrApST (maxId+1)
                                    else if curStatus == 1 then 
                                        let idx = getTaskId task
                                            el = createSubTask idx input
                                        in 
                                            M.continue $ setInputField Nothing $ insertState index (L.listMoveTo (pos + 1) $ L.listInsert (pos + 1) el l) noErrApST
                                    else if curStatus == 2 then 
                                        M.continue $ setInputField Nothing $ insertState index (replaceTask pos (changeTaskContent task input) l) noErrApST
                                    else M.continue noErrApST
                            Nothing  ->  
                                if curStatus == 0 then 
                                        let maxId = getMaxId noErrApST
                                            el = createMainTask index (maxId+1) input
                                        in 
                                            M.continue $ setInputField Nothing $ insertState index (L.listMoveTo 1 $ L.listInsert 0 el l) $ setMaxId noErrApST (maxId+1)
                                    else M.continue noErrApST

                V.EvKey V.KEsc [] ->
                    M.continue $ setInputField Nothing $ noErrApST

                V.EvKey (V.KChar '\BS') []  ->
                    if length input > 0 
                        then
                            -- Assuming you want to remove the last character from the input
                            let newInput = init input
                            in M.continue $ setInputField (Just newInput) noErrApST
                        else
                            -- Handle the case where there is no input to remove
                            M.continue noErrApST
                V.EvKey (V.KChar '\DEL') [] -> 
                    if length input > 0 then let newInput = init input in M.continue $ setInputField (Just newInput) noErrApST else M.continue noErrApST
                V.EvKey  V.KBS [] -> 
                    if length input > 0 then let newInput = init input in M.continue $ setInputField (Just newInput) noErrApST else M.continue noErrApST



                V.EvKey (V.KChar c) [] ->

                    M.continue $ setInputField (Just (input ++ [c])) noErrApST

                -- any other key pressed, we will not handle it
                _ -> M.continue noErrApST

        -- if the inputField is empty, then we will handle the event as usual
        Nothing ->
                case e of

                    V.EvKey (V.KChar '1') [] ->

                        M.continue $ setInputField (Just "") noErrApST {status = 0}

                    V.EvKey (V.KChar '2') [] ->
                        case L.listSelectedElement l of
                            Just (pos,task)  -> 
                                M.continue $ setInputField (Just "") noErrApST {status = 1}
                            Nothing -> M.continue $ noErrApST {errorMessage = Just "Pls fisrt create a main task"}

                    V.EvKey (V.KChar '3') [] ->
                        case L.listSelectedElement l of
                            Just (pos,task)  -> 
                                M.continue $ setInputField (Just (getContent task)) noErrApST {status = 2}
                            Nothing -> M.continue $ noErrApST {errorMessage = Just "To modify, you have to at least choose one task"}


                    V.EvKey (V.KChar '4') [] ->
                        if index == 5 then M.continue noErrApST
                        else 
                            case l^.(L.listSelectedL) of
                                Nothing -> M.continue noErrApST
                                Just pos  -> 
                                    let
                                        doneL = donelist noErrApST
                                        Just (pos, doneTask) = L.listSelectedElement l
                                        -- updatedList = L.listRemove pos l
                                        -- updatedDoneList = L.listInsert 0 doneTask doneL
                                        idx = getTaskId doneTask
                                        
                                    in
                                        if isSub doneTask then 
                                            let 
                                                modifiedT = setTaskDone doneTask
                                            in M.continue $ insertState index (replaceTask pos modifiedT l) noErrApST
                                        else 
                                            let (doneTasks, updatedList) = getDelsTandNewL l idx
                                                updatedDoneList = insertGL doneTasks doneL
                                            in
                                                M.continue $ insertState index updatedList (noErrApST {donelist = updatedDoneList})


                    V.EvKey (V.KChar '5') [] ->
                        case l^.(L.listSelectedL) of
                            Nothing -> M.continue noErrApST
                            Just pos  -> 
                                let
                                    doneL = donelist noErrApST
                                    Just (pos, doneTask) = L.listSelectedElement l
                                    -- updatedList = L.listRemove pos l
                                    -- updatedDoneList = L.listInsert 0 doneTask doneL
                                    idx = getTaskId doneTask
                                    
                                in
                                    if isSub doneTask || index < 5 then M.continue $ noErrApST
                                    else let (tasks, updateDoneL) = getDelsTandNewL doneL idx 
                                            -- modify all the tasks in the given list as undo
                                             toDoTasks = map setTaskToDo tasks
                                             pointedListIndex = getPriority doneTask
                                             updatedTodoL = insertGL toDoTasks $ getList pointedListIndex noErrApST
                                         in 
                                            M.continue $ insertState 5 updateDoneL $ insertState pointedListIndex updatedTodoL noErrApST
                    V.EvKey (V.KChar '+') [] -> M.continue $ noErrApST {theme = theme noErrApST +1}                        
                    V.EvKey (V.KChar '-') [] ->
                        case L.listSelectedElement l of
                            Nothing -> M.continue noErrApST
                            Just (pos,task)  -> 
                                if isSub task then 
                                    let
                                        updatedList =  L.listMoveBy (max (pos-1) 0) $ L.listRemove pos l
                                    in
                                        M.continue $ insertState index updatedList noErrApST        
                                else  
                                    let 
                                        (tasks, updatedList) = getDelsTandNewL l (getTaskId task)
                                    in
                                        M.continue $ insertState index (L.listMoveBy (max (pos-1) 0) updatedList) noErrApST  
                    V.EvKey (V.KChar 'r') [] ->
                        M.continue $ noErrApST { status=5, errorMessage = Just "If you are sure to clear all records, press 'm'"}
                    V.EvKey (V.KDown) [] ->
                        case l^.(L.listSelectedL) of
                            Just pos ->
                                let len = getLen l - 1
                                in  if pos == len then
                                        if index == 5 then M.continue $ insertState index (L.listMoveTo 0 l) noErrApST
                                        else if index /= 4 
                                            then M.continue (noErrApST {pointer = index + 1}) 
                                            else M.continue noErrApST
                                    else M.continue $ insertState index (L.listMoveBy 1 l) noErrApST
                            Nothing ->
                                    if index < 4 then M.continue (noErrApST {pointer = index + 1}) 
                                    else M.continue noErrApST 

                    V.EvKey (V.KUp) [] ->
                        case l^.(L.listSelectedL) of
                            Just pos ->
                                if pos == 0 then
                                    if index == 5 then M.continue $ insertState index (L.listMoveTo (getLen l) l) noErrApST
                                    else if index /= 1 
                                                then M.continue (noErrApST {pointer = index - 1})
                                                else M.continue noErrApST
                                else M.continue $ insertState index (L.listMoveBy (-1) l) noErrApST
                            Nothing ->
                                if index > 1 then M.continue (noErrApST {pointer = index - 1}) 
                                else M.continue noErrApST 
                    
                    V.EvKey (V.KRight) [] ->
                        M.continue (noErrApST {pointer = 5})
            
                    V.EvKey (V.KLeft) [] ->
                        M.continue (noErrApST {pointer = 1})
                    -- V.EvKey (V.KChar '-') [] ->
                    --     case l^.(L.listSelectedL) of
                    --         Nothing -> M.continue l
                    --         Just i  -> M.continue $ L.listRemove i l

                    V.EvKey V.KEsc [] -> M.halt noErrApST

                    -- any other key pressed, we will not handle it
                    _ -> M.continue noErrApST

                -- where
                --     nextElement :: Vec.Vector Char -> Char
                --     nextElement v = fromMaybe '?' $ Vec.find (flip Vec.notElem v) (Vec.fromList ['a' .. 'z'])
                    
    where 
        index = pointer appState
        l = getList index appState
appEvent l _ = M.continue l

-- listDrawElement :: (Show a) => Bool -> a -> Widget ()
-- listDrawElement sel a =
--     let selStr s = if sel
--                    then withAttr customAttr (str $ "<" <> s <> ">")
--                    else str s
--     in C.hCenter $ str "Item " <+> (selStr $ show a)

-- initialState :: L.List () Char
-- initialState = L.list () (Vec.fromList ['a','b']) 2

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
}

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

emptyS :: AppState
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

getPriority :: Task -> Int
getPriority (MUT _) = 1
getPriority (UT  _) = 2
getPriority (IMT _) = 3
getPriority (NNT _) = 4


-- this function takes a list of tasks and a id then return the length of the current task
getTaskLen :: L.List Name Task -> Int -> Int
getTaskLen l idx = go (Vec.toList (L.listElements l )) idx 0
    where 
        go (h:t) id count = if getTaskId h == idx then go t idx (count+1)
                                                   else go t idx count
        go _     _  count = count

-- this function takes into a list and a id then return the index +1 of the last subtask
getTaskEndIndex :: L.List Name Task -> Int -> Int
getTaskEndIndex l idx = go (Vec.toList (L.listElements l )) idx 0 False
    where 
        go (h:t) id count False = if getTaskId h == idx then go t idx (count+1) True
                                                   else go t idx (count+1) False
        go (h:t) id count True  = if getTaskId h == idx then go t idx (count+1) True
                                                   else go t idx (count) True

        go _     _  count _= count 


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


customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listSelectedFocusedAttr, V.black `on` V.white)
    ,(L.listAttr,    V.white `on` V.black)
    ]

theApp :: M.App AppState e Name
theApp = M.App
    { M.appDraw         = drawUI
    , M.appChooseCursor = M.neverShowCursor
    , M.appHandleEvent  = appEvent
    , M.appStartEvent   = return
    , M.appAttrMap      = const theMap
    }




main :: IO ()
main = void $ M.defaultMain theApp initialState