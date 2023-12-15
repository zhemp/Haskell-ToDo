module Event where

import Data
import Utility
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.List as L
import Control.Monad.IO.Class (liftIO)





appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next (AppState))
appEvent appState (T.VtyEvent e) = 
    let noErrApST = appState {errorMessage = Nothing}
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
                        if index == 5 then M.continue $ noErrApST {errorMessage = Just "Pls do not add task into the done list"}
                        else M.continue $ setInputField (Just "") noErrApST {status = 0}

                    V.EvKey (V.KChar '2') [] ->
                        if index == 5 then M.continue $ noErrApST {errorMessage = Just "Pls do not add task into the done list"}
                        else 
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
                    V.EvKey (V.KChar '+') [] -> 
                            M.continue $ noErrApST {theme = theme noErrApST +1}                        
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
                    V.EvKey (V.KChar 'm') [] -> if status noErrApST==5 then M.continue $ emptyS { theme = theme noErrApST, status=0} 
                                                                       else M.continue $ noErrApST
                    V.EvKey (V.KChar 'r') [] ->
                        M.continue $ noErrApST { status=5, errorMessage = Just "If you are sure to clear all records, press 'm'."}
                    V.EvKey (V.KChar 's') [] -> do
                                liftIO $ exportState noErrApST "state.json"
                                M.continue $ noErrApST { status=5, errorMessage = Just "Saved succefully"}
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

                    V.EvKey V.KEsc [] -> do
                        liftIO $ exportState noErrApST "state.json"
                        M.halt $ noErrApST { status=5, errorMessage = Just "Saved succefully"}

                    -- any other key pressed, we will not handle it
                    _ -> M.continue noErrApST

                -- where
                --     nextElement :: Vec.Vector Char -> Char
                --     nextElement v = fromMaybe '?' $ Vec.find (flip Vec.notElem v) (Vec.fromList ['a' .. 'z'])
                    
    where 
        index = pointer appState
        l = getList index appState
appEvent l _ = M.continue l