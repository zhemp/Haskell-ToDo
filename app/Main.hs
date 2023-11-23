{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Main where

import Brick

-- >>> 1+2
-- 3

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui

