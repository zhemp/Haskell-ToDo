module Main where

import Brick

-- >>> 1+1

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui