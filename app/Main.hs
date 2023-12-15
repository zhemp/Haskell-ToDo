{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import UI
import Event
import Data
import Utility

import Control.Monad (void)
import qualified Brick.Main as M





theApp :: M.App AppState e Name
theApp = M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.neverShowCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = selectTheme
    }


main :: IO ()
main =  do
    let filePath = "state.json"

    maybeAppState <- importState filePath
    case maybeAppState of
        Just appState -> void $ M.defaultMain theApp appState
        Nothing -> void $ M.defaultMain theApp initialState



