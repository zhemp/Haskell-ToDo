import Test.HUnit
import Data
import Utility
-- module Spec where

-- Test cases for the getPriority function
getPriorityTests :: Test
getPriorityTests = test
    [ "MUT with different priority" ~:
        getPriority (MUT (5, "Test 5")) @?= 1
    , "UT with zero priority" ~:
        getPriority (UT (0, "Zero Priority")) @?= 2
    , "IMT with high priority" ~:
        getPriority (IMT (999, "High Priority")) @?= 3
    , "NNT with negative priority" ~:
        getPriority (NNT (-1, "Negative Priority")) @?= 4  -- assuming your function handles negative values
    , "UT with empty description" ~:
        getPriority (UT (2, "")) @?= 2
    , "MUT has priority 1" ~:
        getPriority (MUT (1, "Test")) @?= 1
    , "UT has priority 2" ~:
        getPriority (UT (2, "Test")) @?= 2
    , "IMT has priority 3" ~:
        getPriority (IMT (3, "Test")) @?= 3
    , "NNT has priority 4" ~:
        getPriority (NNT (4, "Test")) @?= 4
    ]

-- Test function
testChangeTaskContent :: Test
testChangeTaskContent = TestList [testIMT, testUT, testMUT, testNNT, testSUB]

-- Helper to create test cases
makeTestCase :: (Eq a, Show a) => String -> a -> a -> Test
makeTestCase name expected actual = TestCase (assertEqual name expected actual)

-- Test cases for each task type
testIMT = makeTestCase "IMT Task" (IMT (1, "new content")) (changeTaskContent (IMT (1, "old")) "new content")
testUT  = makeTestCase "UT Task"  (UT  (1, "new content")) (changeTaskContent (UT  (1, "old")) "new content")
testMUT = makeTestCase "MUT Task" (MUT (1, "new content")) (changeTaskContent (MUT (1, "old")) "new content")
testNNT = makeTestCase "NNT Task" (NNT (1, "new content")) (changeTaskContent (NNT (1, "old")) "new content")
testSUB = makeTestCase "SUB Task" (SUB (1, True, "new content")) (changeTaskContent (SUB (1, True, "old")) "new content")



main :: IO ()
main = do
    -- Run the getPriority tests
    putStrLn "Running getPriority tests:"
    _ <- runTestTT getPriorityTests
    runTestTT testChangeTaskContent
    return ()
