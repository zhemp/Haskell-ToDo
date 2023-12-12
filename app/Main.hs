{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V
import Lens.Micro ((^.))

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, vBox, vLimit, withAttr, (<+>), hBox)
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec


-- >>> imList initialState
isSub :: Task -> Bool -- tell whether a task is a subtask
isSub (SUB _) = True
isSub _       = False

drawUI ::  AppState -> [Widget Name]
drawUI appState = [ui]
    where 
        focus = pointer appState --get the current focused list id

        total_mu = Vec.length $ Vec.filter (not . isSub) ((L.listElements) (muList appState)) 
        total_u = Vec.length $ Vec.filter (not . isSub) ((L.listElements) (uList appState)) 
        total_m = Vec.length $ Vec.filter (not . isSub) ((L.listElements) (imList appState)) 
        total_nn = Vec.length $ Vec.filter (not . isSub) ((L.listElements) (nnList appState)) 
        undone_total = str $ show $ (total_mu + total_u + total_m + total_nn)
        total_done = str $ show $ Vec.length $ Vec.filter (not . isSub) ((L.listElements) (donelist appState))   --get the current count of tasks

        ui =  C.vCenter $ vBox [ C.hCenter (str "You have a total of " <+> undone_total <+> str " tasks undone and " <+> total_done <+> str " done"),
                                B.hBorder,
                                hBox [vBox [mubox,
                                            ubox,
                                            mbox,
                                            nnbox
                                            ],
                                        doneBox
                                    ],
                                B.hBorder,
                                -- hBox[
                                --     vLimit 3 $ vBox [C.center (str "add"), B.hBorder, C.center (str "+")],
                                --     B.vBorder,
                                --     vLimit 3 $ vBox [C.center (str "add"), B.hBorder, C.center (str "+")],
                                --     B.vBorder,
                                --     vLimit 3 $ vBox [C.center (str "add"), B.hBorder, C.center (str "+")],
                                --     B.vBorder,
                                --     vLimit 3 $ vBox [C.center (str "add"), B.hBorder, C.center (str "+")]
                                -- ]
                                vLimit 3 $ vBox [
                                    hBox[C.center (str "add"), B.vBorder, C.center (str "add"), B.vBorder, C.center (str "add"), B.vBorder, C.center (str "add")],
                                    B.hBorder,
                                    hBox[C.center (str "+"), B.vBorder, C.center (str "+"), B.vBorder, C.center (str "+"), B.vBorder, C.center (str "+")]
                                ]
                              ]
        mubox = B.borderWithLabel (str "Imp and Urgent") $
                L.renderList listDrawElement (focus == 1) (muList appState)
        ubox = B.borderWithLabel (str "Urgent") $
                L.renderList listDrawElement (focus == 2) (uList appState)
        mbox = B.borderWithLabel (str "Imp") $
                L.renderList listDrawElement (focus == 3) (imList appState)
        nnbox = B.borderWithLabel (str "Not Imp nor Urgent") $
                L.renderList listDrawElement (focus == 4) (nnList appState)
        doneBox = B.borderWithLabel (str "Done") $ 
                L.renderList listDrawElement (focus == 5) (donelist appState)
    --     label = str "Item " <+> cur <+> str " of " <+> total
    --     cur = case l^.(L.listSelectedL) of
    --             Nothing -> str "-"
    --             Just i  -> str (show (i + 1))
    --     total = str $ show $ Vec.length $ l^.(L.listElementsL)
    --     box = B.borderWithLabel label $
    --           hLimit 25 $
    --           vLimit 15 $
    --           L.renderList listDrawElement False l
    --     ui = C.vCenter $ vBox [ C.hCenter box
    --                           , str " "
    --                           , C.hCenter $ str "Press +/- to add/remove list elements."
    --                           , C.hCenter $ str "Press Esc to exit."
    --                           ]

listDrawElement :: (Show a) => Bool -> a -> Widget Name
listDrawElement _ a =
    C.hCenter $ str (show a)

-- listDrawElement :: Bool -> Task -> Widget Name  replace he current draw with this
-- listDrawElement _ task =
--         case task of
--         SUB (_, done, content) -> if done then C.hCenter (str "X " <+> str content)
--                                             else C.hCenter (str content)
--         IMT (_, content) -> C.hCenter $ str content
--         UT  (_, content) -> C.hCenter $ str content
--         MUT (_, content) -> C.hCenter $ str content
--         NNT (_, content) -> C.hCenter $ str content

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next (AppState))
appEvent appState (T.VtyEvent e) =  
    let index = pointer appState
        l = getList index appState
    in 
        case e of
            V.EvKey (V.KChar '+') [] ->
                let 
                    maxId = getMaxId index appState
                    el = createMainTask index (maxId+1) "this is my new String"
                    in 
                    case l^.(L.listSelectedL) of
                        Just pos ->
                                M.continue $ insertState index (L.listMoveTo (pos + 1) $ L.listInsert (pos + 1) el l) (setMaxId index appState (maxId + 1))
                        Nothing ->
                                M.continue $ insertState index (L.listMoveTo 1 $ L.listInsert 0 el l) (setMaxId index appState (maxId + 1))

            V.EvKey (V.KChar '-') [] ->
                case l^.(L.listSelectedL) of
                    Nothing -> M.continue appState
                    Just pos  -> 
                        let
                            updatedList = L.listRemove pos l
                        in
                            M.continue $ insertState index updatedList appState

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
                            M.continue appState 

            V.EvKey (V.KUp) [] ->
                case l^.(L.listSelectedL) of
                    Just pos ->
                        if pos == 0
                            then if index /= 1 && index /= 5
                                        then M.continue (appState {pointer = index - 1})
                                        else M.continue appState
                            else M.continue $ insertState index (L.listMoveBy (-1) l) appState
                    Nothing ->
                        M.continue appState
            
            V.EvKey (V.KRight) [] ->
                M.continue (appState {pointer = 5})
    
            V.EvKey (V.KLeft) [] ->
                M.continue (appState {pointer = 1})
            -- V.EvKey (V.KChar '-') [] ->
            --     case l^.(L.listSelectedL) of
            --         Nothing -> M.continue l
            --         Just i  -> M.continue $ L.listRemove i l

            V.EvKey V.KEsc [] -> M.halt appState

            otherEvent -> M.continue appState
        where
            nextElement :: Vec.Vector Char -> Char
            nextElement v = fromMaybe '?' $ Vec.find (flip Vec.notElem v) (Vec.fromList ['a' .. 'z'])
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


-- (SUB  (represent the id of its main task,  Bool shows whether the subtask has been done, String is whether it has been done))
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
    curMaxId  :: [Int]
}



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

initialState :: AppState
initialState = AppState {
    pointer  = 1,
    status   = 0,
    imList   = L.list Imp (Vec.fromList [(IMT (0, "test")), (SUB (0, True, "line")), (IMT (1, "test")), (IMT (2, "test"))]) 0,
    uList    = L.list Urg (Vec.fromList [(UT (0, "test")), (SUB (0, True, "line")), (UT (1, "test")), (UT (2, "test"))]) 0,
    muList   = L.list Impurg (Vec.fromList [(MUT (0, "test")), (SUB (0, True, "line")), (MUT (1, "test")), (MUT (2, "test"))]) 0,
    nnList   = L.list Nn (Vec.fromList [(NNT (0, "test")), (SUB (0, True, "line")), (NNT (1, "test")), (NNT (2, "test"))]) 0,
    donelist = L.list Done (Vec.empty) 0,
    curMaxId = [2,2,2,2,0]
        }

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