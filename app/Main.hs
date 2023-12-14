{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import UI
import Event
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




theApp :: M.App AppState e Name
theApp = M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.neverShowCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = \s -> let value = theme s in 
                        case value `mod` 4 of 
                            0 -> defaultThemeMap
                            1 -> violetThemeMap
                            2 -> darkvioletThemeMap
                            3 -> pearThemeMap
    }

 
main :: IO ()
main =  do
    let filePath = "state.json"

    maybeAppState <- importState filePath
    case maybeAppState of
        Just appState -> void $ M.defaultMain theApp appState
        Nothing -> void $ M.defaultMain theApp initialState
    

