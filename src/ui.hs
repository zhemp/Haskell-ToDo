module UI where

import Data
import Utility
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



drawUI ::  AppState -> [Widget Name]
drawUI appState = [ui]
    where
        index_map = genIdToRankM appState
        errmsg = errorMessage appState

        cur_theme = case (theme appState) `mod` 4 of
            0 -> "Default" 
            1 -> "Violet"
            2 -> "Dark Violet"
            3 -> "Pear"

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

        mubox = if focus == 1 then withAttr borderFocusedAttr $ B.borderWithLabel (str ("Imp and Urgent: " ++ show total_mu ++ " main tasks " ++ show total_mu_sub ++ " sub-tasks")) $
                                withAttr muAttr $ L.renderList (listDrawElement index_map (focus == 1)) (focus == 1) (muList appState)
                            else B.borderWithLabel (str ("Imp and Urgent: " ++ show total_mu ++ " main tasks " ++ show total_mu_sub ++ " sub-tasks")) $
                                withAttr muAttr $ L.renderList (listDrawElement index_map (focus == 1)) (focus == 1) (muList appState)
        ubox = if focus == 2 then withAttr borderFocusedAttr $ B.borderWithLabel (str ("Urgent: " ++ show total_u ++ " main tasks " ++ show total_u_sub ++ " sub-tasks")) $
                                withAttr uAttr $ L.renderList (listDrawElement index_map (focus == 2)) (focus == 2) (uList appState)
                            else B.borderWithLabel (str ("Urgent: " ++ show total_u ++ " main tasks " ++ show total_u_sub ++ " sub-tasks")) $
                                withAttr uAttr $ L.renderList (listDrawElement index_map (focus == 2)) (focus == 2) (uList appState)
        mbox = if focus == 3 then withAttr borderFocusedAttr $ B.borderWithLabel (str ("Imp: " ++ show total_m ++ " main tasks " ++ show total_m_sub ++ " sub-tasks")) $
                                withAttr mAttr $ L.renderList (listDrawElement index_map (focus == 3)) (focus == 3) (imList appState)
                            else B.borderWithLabel (str ("Imp: " ++ show total_m ++ " main tasks " ++ show total_m_sub ++ " sub-tasks")) $
                                withAttr mAttr $ L.renderList (listDrawElement index_map (focus == 3)) (focus == 3) (imList appState)
        nnbox = if focus == 4 then withAttr borderFocusedAttr $ B.borderWithLabel (str ("Not imp nor urgent: " ++ show total_nn ++ " main tasks " ++ show total_nn_sub ++ " sub-tasks")) $
                                withAttr nnAttr $ L.renderList (listDrawElement index_map (focus == 4)) (focus == 4) (nnList appState)
                            else B.borderWithLabel (str ("Not imp nor urgent: " ++ show total_nn ++ " main tasks " ++ show total_nn_sub ++ " sub-tasks")) $
                                withAttr nnAttr $ L.renderList (listDrawElement index_map (focus == 4)) (focus == 4) (nnList appState)
        doneBox = if focus == 5 then withAttr borderFocusedAttr $ B.borderWithLabel (str ("Done: " ++ show total_done ++ " tasks")) $ 
                                L.renderList (listDrawElement index_map (focus == 5)) (focus == 5) (donelist appState)
                            else B.borderWithLabel (str ("Done: " ++ show total_done ++ " tasks")) $ 
                                L.renderList (listDrawElement index_map (focus == 5)) (focus == 5) (donelist appState)

        ui = case inputField appState of
            Nothing -> C.hCenter $ C.vCenter $ hLimit 130 $ vLimit 50 $ B.borderWithLabel (str "Fantastic To-do") $ 
                C.vCenter $ vBox [ C.hCenter (str "You have a total of " <+> undone_total <+> str " tasks undone and " <+> str (show total_done) <+> str " done. " <+> str "Current theme is: " <+> str cur_theme),
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
                                                        , C.center $ str "Press any key to ignore."
                                                    ]
                                    Nothing -> emptyWidget
                                ]
            Just input -> C.hCenter $ C.vCenter $ hLimit 131 $ vLimit 50 $ B.borderWithLabel (str "Fantastic To-do") $ 
                C.vCenter $ vBox [ C.hCenter (str "You have a total of " <+> undone_total <+> str " tasks undone and " <+> str (show total_done) <+> str " done. " <+> str "Current theme is: " <+> str cur_theme),
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

listDrawElement :: M.Map Int Int -> Bool -> Bool -> Task -> Widget Name --replace he current draw with this
listDrawElement map focused selected task =
        case task of
        SUB (_, done, content) -> if selected && focused then
                                        withAttr selectedFocusedAttr $  if done then str "  └── " <+> str (concatMap (\c -> [c, '\x0336']) content)
                                                    else str "  └── " <+> str content 
                                    else
                                        if done then str "  └── " <+> str (concatMap (\c -> [c, '\x0336']) content)
                                                    else str "  └── " <+> str content 
        IMT (id, content) -> let Just index = (M.lookup id map) 
                            in if selected && focused then withAttr selectedFocusedAttr $ str (show index) <+> str "." <+> str content
                                                        else str (show index) <+> str "." <+> str content
        UT  (id, content) -> let Just index = (M.lookup id map) 
                            in if selected && focused then withAttr selectedFocusedAttr $ str (show index) <+> str "." <+> str content
                                                        else str (show index) <+> str "." <+> str content
        MUT (id, content) -> let Just index = (M.lookup id map) 
                            in if selected && focused then withAttr selectedFocusedAttr $ str (show index) <+> str "." <+> str content
                                                        else str (show index) <+> str "." <+> str content
        NNT (id, content) -> let Just index = (M.lookup id map) 
                            in if selected && focused then withAttr selectedFocusedAttr $ str (show index) <+> str "." <+> str content
                                                        else str (show index) <+> str "." <+> str content



violetThemeMap :: A.AttrMap
violetThemeMap = A.attrMap V.defAttr -- white Theme
    [ (muAttr, V.black `on` (V.rgbColor 185 251 192)) 
    , (uAttr,  V.black `on` (V.rgbColor 142 236 245))  
    , (mAttr,  V.black `on` (V.rgbColor 163 196 243))  
    , (nnAttr, V.black `on` (V.rgbColor 241 192 232) )  
    , (selectedFocusedAttr, V.black `on` V.brightWhite)
    , (borderFocusedAttr, fg (V.rgbColor 255 0 0))
    ]

darkvioletThemeMap :: A.AttrMap
darkvioletThemeMap = A.attrMap V.defAttr
    [ (muAttr, V.black `on` (V.rgbColor 255 203 242)) -- Example colors
    , (uAttr,  V.black `on` (V.rgbColor 224 170 255))
    , (mAttr,  V.black `on` (V.rgbColor 199 125 255))
    , (nnAttr, V.black `on` (V.rgbColor 157 78 221) )
    , (selectedFocusedAttr, V.black `on` V.brightWhite)
    , (borderFocusedAttr, fg (V.rgbColor 255 0 0))
    ]

pearThemeMap :: A.AttrMap
pearThemeMap = A.attrMap V.defAttr
    [ (muAttr, V.black `on` (V.rgbColor 251 196 171)) -- Example colors
    , (uAttr,  V.black `on` (V.rgbColor 248 173 157))
    , (mAttr,  V.black `on` (V.rgbColor 244 151 142))
    , (nnAttr, V.black `on` (V.rgbColor 240 128 128) )
    , (selectedFocusedAttr, V.black `on` V.brightWhite)
    , (borderFocusedAttr, fg (V.rgbColor 255 0 0))
    ]

defaultThemeMap :: A.AttrMap
defaultThemeMap = A.attrMap V.defAttr
    [ (muAttr, V.defAttr)   -- Example colors
    , (uAttr,  V.defAttr)  
    , (mAttr,  V.defAttr)  
    , (nnAttr, V.defAttr)  
    , (selectedFocusedAttr, V.black `on` V.brightWhite)
    , (borderFocusedAttr, fg (V.rgbColor 255 0 0))
    ]

-- defaultThemeMap :: A.AttrMap
-- defaultThemeMap = A.attrMap V.defAttr -- white Theme
--     [ (muAttr, V.black `on` (V.rgbColor 251 196 171))   -- Example colors
--     , (uAttr,  V.black `on` (V.rgbColor 248 173 157))  
--     , (mAttr,  V.black `on` (V.rgbColor 244 151 142))  
--     , (nnAttr, V.black `on` (V.rgbColor 240 128 128))  
--     , (selectedFocusedAttr, V.black `on` V.brightWhite)
--     ]

muAttr, uAttr, mAttr, nnAttr, selectedFocusedAttr, borderFocusedAttr:: A.AttrName
muAttr = A.attrName "muListAttr"
uAttr  = A.attrName "uListAttr"
mAttr  = A.attrName "mListAttr"
nnAttr = A.attrName "nnListAttr"
selectedFocusedAttr = A.attrName "selectedFocusedAttr"
borderFocusedAttr = A.attrName "borderFocusedAttr"

-- selectTheme :: AppState -> A.AttrMap
-- selectTheme appState = 
--     let currentTheme = theme appState
--     in if currentTheme == 0 then defaultThemeMap else if currentTheme == 1 then lightThemeMap else darkThemeMap