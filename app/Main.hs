{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
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

drawUI ::  AppState -> [Widget ()]
drawUI l = [ui]
    where 
        ui =  C.vCenter $ vBox [ C.hCenter (str "Count"),
                                B.hBorder,
                                hBox [vBox [mubox,
                                            ubox,
                                            mbox,
                                            nnbox
                                            ],
                                        doneBox
                                    ],
                                B.hBorder,
                                hBox[
                                    vBox [C.center (str "add"), B.hBorder, C.center(str "+")],
                                    B.vBorder,
                                    vBox [C.center (str "add"), B.hBorder, C.center(str "+")],
                                    B.vBorder,
                                    vBox [C.center (str "add"), B.hBorder, C.center(str "+")],
                                    B.vBorder
                                    vBox [C.center (str "add"), B.hBorder, C.center(str "+")],
                                ]
                              ]
        mubox = B.borderWithLabel (str "Imp and Urgent") $
                L.renderList listDrawElement True l
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

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next (AppState))
appEvent l (T.VtyEvent e) = undefined
--     case e of
--         V.EvKey (V.KChar '+') [] ->
--             let el = nextElement (L.listElements l)
--                 pos = Vec.length $ l^.(L.listElementsL)
--             in M.continue $ L.listInsert pos el $ L.listInsert pos el l

--         V.EvKey (V.KChar '-') [] ->
--             case l^.(L.listSelectedL) of
--                 Nothing -> M.continue l
--                 Just i  -> M.continue $ L.listRemove i l

--         V.EvKey V.KEsc [] -> M.halt l

--         ev -> M.continue =<< (L.handleListEventVi L.handleListEvent) ev l
--     where
--       nextElement :: Vec.Vector Char -> Char
--       nextElement v = fromMaybe '?' $ Vec.find (flip Vec.notElem v) (Vec.fromList ['a' .. 'z'])
-- appEvent l _ = M.continue l

-- listDrawElement :: (Show a) => Bool -> a -> Widget ()
-- listDrawElement sel a =
--     let selStr s = if sel
--                    then withAttr customAttr (str $ "<" <> s <> ">")
--                    else str s
--     in C.hCenter $ str "Item " <+> (selStr $ show a)

-- initialState :: L.List () Char
-- initialState = L.list () (Vec.fromList ['a','b']) 2

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App AppState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }
type L1Task = (Int, String) --()

data Task = 
   IMT L1Task
 | UT L1Task
 | MUT L1Task
 | NNT L1Task
 | SUB (Int,Bool, String) 
 deriving (Show)


data AppState = AppState {
    pointer   :: Int,
    imList    :: L.List Int Task,
    uList     :: L.List Int Task, 
    muList    :: L.List Int Task, 
    nnList    :: L.List Int Task,
    donelist  :: L.List Int Task
}

initialState :: AppState
initialState = AppState {
    pointer  =                              0,
    imList   = L.list 0 (Vec.fromList [(IMT (0, "test")), (SUB (0, True, "line")), (IMT (1, "test")), (IMT (2, "test"))]) 0,
    uList    = L.list 0 (Vec.fromList [(UT (0, "test")), (SUB (0, True, "line")), (UT (1, "test")), (UT (2, "test"))]) 0,
    muList   = L.list 0 (Vec.fromList [(MUT (0, "test")), (SUB (0, True, "line")), (MUT (1, "test")), (MUT (2, "test"))]) 0,
    nnList   = L.list 0 (Vec.fromList [(NNT (0, "test")), (SUB (0, True, "line")), (NNT (1, "test")), (NNT (2, "test"))]) 0,
    donelist = L.list 0 (Vec.empty) 0
        }
        


customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App AppState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.neverShowCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }



main :: IO ()
main = void $ M.defaultMain theApp initialState