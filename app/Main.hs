{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
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




drawUI ::  AppState -> [Widget Name]
drawUI appState = [ui]
    where 
        errorMessage = errorMessage appState
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
                L.renderList listDrawElement (focus == 1) (muList appState)
        ubox = B.borderWithLabel (str ("Urgent: " ++ show total_u ++ " main tasks " ++ show total_u_sub ++ " sub-tasks")) $
                L.renderList listDrawElement (focus == 2) (uList appState)
        mbox = B.borderWithLabel (str ("Imp: " ++ show total_m ++ " main tasks " ++ show total_m_sub ++ " sub-tasks")) $
                L.renderList listDrawElement (focus == 3) (imList appState)
        nnbox = B.borderWithLabel (str ("Not imp nor urgent: " ++ show total_nn ++ " main tasks " ++ show total_nn_sub ++ " sub-tasks")) $
                L.renderList listDrawElement (focus == 4) (nnList appState)
        doneBox = B.borderWithLabel (str ("Done: " ++ show total_done ++ " tasks")) $ 
                L.renderList listDrawElement (focus == 5) (donelist appState)

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
                                case errorMessage of
                                    Just error -> vLimit 3 $ vBox[
                                                        C.center $ str error
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
                                ]
            

listDrawElement :: Bool -> Task -> Widget Name
listDrawElement _ task =
    case task of 
        SUB (_, _, _) -> str "  └── " <+> str (show task)
        _ -> str (show task)
-- listDrawElement :: Bool -> Task -> Widget Name --replace he current draw with this
-- listDrawElement _ task =
--         case task of
--         SUB (_, done, content) -> if done then str "  └── " <+> str "X " <+> str content
--                                             else str "  └── " <+> str content
--         IMT (_, content) -> str content
--         UT  (_, content) -> str content
--         MUT (_, content) -> str content
--         NNT (_, content) -> str content



appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next (AppState))
appEvent appState (T.VtyEvent e) = 
    case inputField appState of
        --  if the inputField is not empty, then we will handle the input
        Just input -> 
             case e of
                V.EvKey V.KEnter [] -> 
                    M.continue $ setInputField Nothing $ insertState index (L.listMoveTo (pos + 1) $ L.listInsert (pos + 1) el l) $ setMaxId index appState (maxId+1)
                    where 
                        maxId = getMaxId index appState
                        el = createMainTask index (maxId+1) input
                        Just pos = l^.(L.listSelectedL)
                
                V.EvKey (V.KChar c) [] ->
                    -- add the char to the inputField
                    -- M.continue $ appState { inputField = Just (input ++ [c]) }
                    -- try to visualize the inputField when receiving input, using ListInsert
                    -- let 
                    --     el = createMainTask index maxId (input ++ [c])
                    --     maxId = getMaxId index appState
                    -- in
                    --     case l^.(L.listSelectedL) of
                    --         Just pos ->
                    --                 M.continue $ setInputField (Just (input ++ [c])) $ insertState index (L.listMoveTo (pos + 1)  $ L.listInsert pos el $ L.listRemove pos l) appState
                    --         Nothing ->
                    --                 M.continue $ setInputField (Just (input ++ [c])) $ insertState index (L.listMoveTo (0 + 1) $ L.listInsert 0 el $ L.listRemove 0 l) appState
                    M.continue $ setInputField (Just (input ++ [c])) appState

                -- any other key pressed, we will not handle it
                _ -> M.continue appState

        -- if the inputField is empty, then we will handle the event as usual
        Nothing ->
                case e of

                    V.EvKey (V.KChar '1') [] ->
                        -- let 
                        --     maxId = getMaxId index appState
                        --     el = createMainTask index (maxId+1) ""
                        --     in 
                            -- case L.listSelectedElement l of
                            --     Just (pos,task) ->
                            --         let newPos = pos + getTaskLen l (getTaskId task)
                            --         in
                            --             M.continue $ setInputField (Just "") appstate
                            --     Nothing ->
                            --             M.continue $ setInputField (Just "") appstate
                        M.continue $ setInputField (Just "") appState

                    V.EvKey (V.KChar '2') [] ->
                        case L.listSelectedElement l of
                            Nothing -> M.continue appState
                            Just (pos, task) ->
                                let 
                                    idx = getTaskId task
                                    el = createSubTask idx ""
                                    in 
                                    case l^.(L.listSelectedL) of
                                        Just pos ->
                                                M.continue $ setInputField (Just "") $ insertState index (L.listMoveTo (pos + 1) $ L.listInsert (pos + 1) el l) appState
                                        Nothing ->
                                                M.continue $ setInputField (Just "") $ insertState index (L.listMoveTo 1 $ L.listInsert 0 el l) appState

                    -- V.EvKey (V.KChar '-') [] ->
                    --     case l^.(L.listSelectedL) of
                    --         Nothing -> M.continue appState
                    --         Just pos  -> 
                    --             let
                    --                 updatedList = L.listRemove pos l
                    --             in
                    --                 M.continue $ insertState index updatedList appState        
                    V.EvKey (V.KChar '-') [] ->
                        case L.listSelectedElement l of
                            Nothing -> M.continue appState
                            Just (pos,task)  -> 
                                if isSub task then 
                                    let
                                        updatedList =  L.listMoveBy (max (pos-1) 0) $ L.listRemove pos l
                                    in
                                        M.continue $ insertState index updatedList appState        
                                else  
                                    let 
                                        (tasks, updatedList) = getDelsTandNewL l (getTaskId task)
                                    in
                                        M.continue $ insertState index (L.listMoveBy (max (pos-1) 0) updatedList) appState  

                    V.EvKey (V.KChar '5') [] ->
                        case l^.(L.listSelectedL) of
                            Nothing -> M.continue appState
                            Just pos  -> 
                                let
                                    doneL = donelist appState
                                    Just (pos, doneTask) = L.listSelectedElement l
                                    -- updatedList = L.listRemove pos l
                                    -- updatedDoneList = L.listInsert 0 doneTask doneL
                                    idx = getTaskId doneTask
                                    
                                    
                                in
                                    if isSub doneTask || index < 5 then M.continue $ appState
                                    else let (tasks, updateDoneL) = getDelsTandNewL doneL idx 
                                            -- modify all the tasks in the given list as undo
                                             toDoTasks = map setTaskToDo tasks
                                             pointedListIndex = getPriority doneTask
                                             updatedTodoL = insertGL toDoTasks $ getList pointedListIndex appState
                                         in 
                                            M.continue $ insertState 5 updateDoneL $ insertState pointedListIndex updatedTodoL appState
                                            
                    

                    V.EvKey (V.KChar '4') [] ->
                        case l^.(L.listSelectedL) of
                            Nothing -> M.continue appState
                            Just pos  -> 
                                let
                                    doneL = donelist appState
                                    Just (pos, doneTask) = L.listSelectedElement l
                                    -- updatedList = L.listRemove pos l
                                    -- updatedDoneList = L.listInsert 0 doneTask doneL
                                    idx = getTaskId doneTask
                                    
                                in
                                    if isSub doneTask then 
                                        let 
                                            modifiedT = setTaskDone doneTask
                                        in M.continue $ insertState index (replaceTask pos modifiedT l) appState
                                    else 
                                        let (doneTasks, updatedList) = getDelsTandNewL l idx
                                            updatedDoneList = insertGL doneTasks doneL
                                         in
                                            M.continue $ insertState index updatedList (appState {donelist = updatedDoneList})
                                            



                    V.EvKey (V.KDown) [] ->
                        case l^.(L.listSelectedL) of
                            Just pos ->
                                let len = getLen l - 1
                                in if pos == len
                                    then if index /= 4 && index /= 5
                                        then M.continue (appState {pointer = index + 1}) 
                                        else M.continue appState
                                    else M.continue $ insertState index (L.listMoveBy 1 l) appState
                            Nothing ->
                                    if index < 4 then M.continue (appState {pointer = index + 1}) 
                                    else M.continue appState 

                    V.EvKey (V.KUp) [] ->
                        case l^.(L.listSelectedL) of
                            Just pos ->
                                if pos == 0
                                    then if index /= 1 && index /= 5
                                                then M.continue (appState {pointer = index - 1})
                                                else M.continue appState
                                    else M.continue $ insertState index (L.listMoveBy (-1) l) appState
                            Nothing ->
                                if index > 1 then M.continue (appState {pointer = index - 1}) 
                                else M.continue appState 
                    
                    V.EvKey (V.KRight) [] ->
                        M.continue (appState {pointer = 5})
            
                    V.EvKey (V.KLeft) [] ->
                        M.continue (appState {pointer = 1})
                    -- V.EvKey (V.KChar '-') [] ->
                    --     case l^.(L.listSelectedL) of
                    --         Nothing -> M.continue l
                    --         Just i  -> M.continue $ L.listRemove i l

                    V.EvKey V.KEsc [] -> M.halt appState

                    -- any other key pressed, we will not handle it
                    _ -> M.continue appState

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
    status    :: Int,
    imList    :: L.List Name Task,
    uList     :: L.List Name Task, 
    muList    :: L.List Name Task, 
    nnList    :: L.List Name Task,
    donelist  :: L.List Name Task,
    curMaxId  :: [Int],
    inputField :: Maybe String  --  used to store the input string
}


-- replace the tasks at the appointed positon of the given list using the given task
replaceTask :: Int -> Task -> L.List Name Task -> L.List Name Task
replaceTask idx modifiedT l = L.listMoveBy (1) $ L.listInsert (idx) modifiedT $ L.listRemove idx l


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

initialState :: AppState
initialState = AppState {
    pointer  = 1,
    status   = 0,
    imList   = L.list Imp    (Vec.fromList [(IMT (0, "test")), (SUB (0, True, "line")), (IMT (1, "test")), (IMT (2, "test"))]) 0,
    uList    = L.list Urg    (Vec.fromList [(UT (0, "test")), (SUB (0, True, "line")), (UT (1, "test")), (UT (2, "test"))]) 0,
    muList   = L.list Impurg (Vec.fromList [(MUT (0, "test")), (SUB (0, False, "line1")),(SUB (0, False, "line2")), (MUT (1, "test")), (MUT (2, "test"))]) 0,
    nnList   = L.list Nn     (Vec.fromList [(NNT (0, "test")), (SUB (0, True, "line")), (NNT (1, "test")), (NNT (2, "test"))]) 0,
    donelist = L.list Done   (Vec.empty) 0,
    curMaxId = [2,2,2,2,0],
    inputField = Nothing
        }

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
getMaxId ::  Int -> AppState -> Int
getMaxId index s = curMaxId s !! (index -1 )



-- given index, appstate, and new maxId   return a updated appstate
-- here the reason why we decrease the index by 1 is because the gap between the index of 5 task list and index of general list
setMaxId :: Int -> AppState -> Int -> AppState
setMaxId index s newMaxId = s { curMaxId = udList (curMaxId s) (index-1) newMaxId}


udList :: (Eq a1, Num a1) => [a2] -> a1 -> a2 -> [a2]
udList (h:t) 0 v = v:t
udList (h:t) id v = h: udList t (id-1) v
udList _ _ v = [v]

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listSelectedFocusedAttr, V.black `on` V.white)
    ,(L.listAttr,    V.white `on` V.black)
    ]

theApp :: M.App AppState e Name
theApp = M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.neverShowCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const theMap
    }




main :: IO ()
main = void $ M.defaultMain theApp initialState