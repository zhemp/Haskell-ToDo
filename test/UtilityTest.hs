import Data
import Utility
import Test.Hspec

import Data
import qualified Data.Map as M
import Control.Monad (void)
import Data.Maybe (fromMaybe, catMaybes)

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


-- Test cases for the replaceTask function
-- replaceTask :: Int -> Task -> L.List Name Task -> L.List Name Task
replaceTaskTests :: SpecWith ()
replaceTaskTests = do
    describe "replaceTask" $ do
        it "should replace the task with the given id with the given task" $ do
            let taskList = L.list Imp (Vec.fromList [IMT (1, "task1"), IMT (2, "task2")]) 1
            let newTask = IMT (1, "newTask")
            let newList= replaceTask 1 newTask taskList 
            L.listElements newList `shouldBe` Vec.fromList [IMT (1, "task1") ,newTask]
        it "should not replace any tasks if the given id is not found" $ do
            let taskList = L.list Imp (Vec.fromList [IMT (1, "task1"), IMT (2, "task2")]) 1
            let newTask = IMT (3, "newTask")
            let newList= replaceTask 3 newTask taskList 
            L.listElements newList `shouldBe` Vec.fromList [IMT (1, "task1"), IMT (2, "task2")]
        it "should not replace any tasks if the given list is empty" $ do
            let taskList = L.list Imp (Vec.fromList []) 1
            let newTask = IMT (3, "newTask")
            let newList= replaceTask 3 newTask taskList 
            L.listElements newList `shouldBe` Vec.fromList []

-- Test cases for the changeTaskContent function
-- changeTaskContent :: Task -> String -> Task 
changeTaskContentTests :: SpecWith ()
changeTaskContentTests = do
    describe "changeTaskContent" $ do
        it "should change the content of the given task" $ do
            let task = IMT (1, "task1")
            let newTask = changeTaskContent task "newTask"
            newTask `shouldBe` IMT (1, "newTask")
        it "should change the content of the given task if the given task is a subtask" $ do
            let task = SUB (1, True, "task1")
            let newTask = changeTaskContent task "newTask"
            newTask `shouldBe` SUB (1, True, "newTask")


-- this function takes into a task and return a string
-- getContent :: Task -> String 
getContentTests :: SpecWith ()
getContentTests = do
    describe "getContent" $ do
        it "should return the content of the given task" $ do
            let task = IMT (1, "task1")
            let content = getContent task
            content `shouldBe` "task1"
        it "should return the content of the given task if the given task is a subtask" $ do
            let task = SUB (1, True, "task1")
            let content = getContent task
            content `shouldBe` "task1"

-- getPriority :: Task -> Int
getPriorityTests :: SpecWith ()
getPriorityTests = do
    describe "getPriority" $ do
        it "should return the priority of the IMT task" $ do
            let task = IMT (1, "task1")
            let priority = getPriority task
            priority `shouldBe` 3
        it "should return the priority of the UT task" $ do
            let task = UT (1, "task1")
            let priority = getPriority task
            priority `shouldBe` 2
        it "should return the priority of the MUT task" $ do
            let task = MUT (1, "task1")
            let priority = getPriority task
            priority `shouldBe` 1
        it "should return the priority of the NNT task" $ do
            let task = NNT (1, "task1")
            let priority = getPriority task
            priority `shouldBe` 4
        
-- getTaskLen :: L.List Name Task -> Int -> Int
getTaskLenTests :: SpecWith ()
getTaskLenTests = do
    describe "getTaskLen" $ do
        it "should return the length of the given task" $ do
            let taskList = L.list Imp (Vec.fromList [IMT (1, "task1"), IMT (2, "task2")]) 1
            let length = getTaskLen taskList 1
            length `shouldBe` 1
        it "should return the length of the given task if the given task is a subtask" $ do
            let taskList = L.list Imp (Vec.fromList [IMT (1, "task1"), SUB (1, True, "task2")]) 1
            let length = getTaskLen taskList 1
            length `shouldBe` 2
        it "should return 0 if the given task is not found" $ do
            let taskList = L.list Imp (Vec.fromList [IMT (1, "task1"), IMT (2, "task2")]) 1
            let length = getTaskLen taskList 3
            length `shouldBe` 0
        it "should return 0 if the given list is empty" $ do
            let taskList = L.list Imp (Vec.fromList []) 1
            let length = getTaskLen taskList 1
            length `shouldBe` 0

-- this function takes into a list and a id then return the index +1 of the last subtask
-- getTaskEndIndex :: L.List Name Task -> Int -> Int
getTaskEndIndexTests :: SpecWith ()
getTaskEndIndexTests = do
    describe "getTaskEndIndex" $ do
        it "should return the index +1 of the last subtask" $ do
            let taskList = L.list Imp (Vec.fromList [IMT (1, "task1"), SUB (1, True, "task2")]) 1
            let index = getTaskEndIndex taskList 1
            index `shouldBe` 2
        it "should return the length of the list if the given index doesn't exist" $ do
            let taskList = L.list Imp (Vec.fromList [IMT (1, "task1"), SUB (1, True, "task2")]) 1
            let index = getTaskEndIndex taskList 3
            index `shouldBe` 2
        it "should return 0 if the given list is empty" $ do
            let taskList = L.list Imp (Vec.fromList []) 1
            let index = getTaskEndIndex taskList 1
            index `shouldBe` 0

-- This function takes a list of tasks and insert it in to the head of another list
-- insertGL ::  [Task] -> L.List Name Task -> L.List Name Task
insertGLTests :: SpecWith ()
insertGLTests = do
    describe "insertGL" $ do
        it "should insert the given list of tasks into the head of the given list" $ do
            let taskList = L.list Imp (Vec.fromList [IMT (1, "task1"), SUB (1, True, "task2")]) 1
            let newList = insertGL [IMT (2, "task3"), SUB (2, True, "task4")] taskList
            L.listElements newList `shouldBe` Vec.fromList [IMT (2, "task3"), SUB (2, True, "task4"), IMT (1, "task1"), SUB (1, True, "task2")]
        it "should insert the given list of tasks into the head of the given list if the given list is empty" $ do
            let taskList = L.list Imp (Vec.fromList []) 1
            let newList = insertGL [IMT (2, "task3"), SUB (2, True, "task4")] taskList
            L.listElements newList `shouldBe` Vec.fromList [IMT (2, "task3"), SUB (2, True, "task4")]
        it "should insert the given list of tasks into the head of the given list if the given list is empty" $ do
            let taskList = L.list Imp (Vec.fromList [IMT (1, "task1"), SUB (1, True, "task2")]) 1
            let newList = insertGL [] taskList
            L.listElements newList `shouldBe` Vec.fromList [IMT (1, "task1"), SUB (1, True, "task2")]

--  THis function takes the pointer (Int) and return the corresponding list
-- getList :: Int -> AppState -> L.List Name Task
getListTests :: SpecWith ()
getListTests = do
    describe "getList" $ do
        it "should return the corresponding list" $ do
            let appState = AppState {
                pointer  = 1,
                status   = 0,
                theme    = 0,
                imList   = L.list Imp    (Vec.fromList [(IMT (0, "test")), (SUB (0, True, "line")), (IMT (1, "test")), (IMT (2, "test"))]) 0,
                uList    = L.list Urg    (Vec.fromList [(UT (3, "test")), (SUB (3, True, "line")), (UT (4, "test")), (UT (5, "test"))]) 0,
                muList   = L.list Impurg (Vec.fromList [(MUT (6, "test")), (SUB (6, False, "line1")),(SUB (6, False, "line2")), (MUT (7, "test")), (MUT (8, "test"))]) 0,
                nnList   = L.list Nn     (Vec.fromList [(NNT (9, "test")), (SUB (9, True, "line")), (NNT (10, "test")), (NNT (11, "test"))]) 0,
                donelist = L.list Done   (Vec.empty) 0,
                curMaxId = 11,
                inputField = Nothing,
                errorMessage = Nothing
                    }
            let list = getList 1 appState
            L.listElements list `shouldBe` Vec.fromList [(MUT (6, "test")), (SUB (6, False, "line1")),(SUB (6, False, "line2")), (MUT (7, "test")), (MUT (8, "test"))]
        it "should return the corresponding list if the given pointer is 2" $ do
            let appState = AppState {
                pointer  = 2,
                status   = 0,
                theme    = 0,
                imList   = L.list Imp    (Vec.fromList [(IMT (0, "test")), (SUB (0, True, "line")), (IMT (1, "test")), (IMT (2, "test"))]) 0,
                uList    = L.list Urg    (Vec.fromList [(UT (3, "test")), (SUB (    3, True, "line")), (UT (4, "test")), (UT (5, "test"))]) 0,
                muList   = L.list Impurg (Vec.fromList [(MUT (6, "test")), (SUB (6, False, "line1")),(SUB (6, False, "line2")), (MUT (7, "test")), (MUT (8, "test"))]) 0,
                nnList   = L.list Nn     (Vec.fromList [(NNT (9, "test")), (SUB (9, True, "line")), (NNT (10, "test")), (NNT (11, "test"))]) 0,
                donelist = L.list Done   (Vec.empty) 0,
                curMaxId = 11,
                inputField = Nothing,
                errorMessage = Nothing
                    }
            let list = getList 2 appState
            L.listElements list `shouldBe` Vec.fromList [(UT (3, "test")), (SUB (3, True, "line")), (UT (4, "test")), (UT (5, "test"))]

-- Input: index, maxId, text, and output Task
-- createMainTask :: Int -> Int -> String -> Task
createMainTaskTests :: SpecWith ()
createMainTaskTests = do
    describe "createMainTask" $ do
        it "should create a IMT task" $ do
            let task = createMainTask 3 1 "test"
            task `shouldBe` IMT (1, "test")
        it "should create a UT task" $ do
            let task = createMainTask 2 1 "test"
            task `shouldBe` UT (1, "test")
        it "should create a MUT task" $ do
            let task = createMainTask 1 1 "test"
            task `shouldBe` MUT (1, "test")
        it "should create a NNT task" $ do
            let task = createMainTask 4 1 "test"
            task `shouldBe` NNT (1, "test")


main :: IO ()
main = hspec $ do
    replaceTaskTests
    changeTaskContentTests
    getContentTests
    getPriorityTests
    getTaskLenTests
    getTaskEndIndexTests
    insertGLTests
    getListTests
    createMainTaskTests







