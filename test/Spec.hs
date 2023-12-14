import Test.HUnit
import Data
import Utility
-- module Spec where

-- Test cases for the getPriority function
getPriorityTests :: Test
getPriorityTests = test
    [ "MUT has priority 1" ~:
        getPriority (MUT (1, "Test")) @?= 1
    , "UT has priority 2" ~:
        getPriority (UT (2, "Test")) @?= 2
    , "IMT has priority 3" ~:
        getPriority (IMT (3, "Test")) @?= 3
    , "NNT has priority 4" ~:
        getPriority (NNT (4, "Test")) @?= 4
    ]

main :: IO ()
main = do
    -- Run the getPriority tests
    putStrLn "Running getPriority tests:"
    _ <- runTestTT getPriorityTests
    return ()
