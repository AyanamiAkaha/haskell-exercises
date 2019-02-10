import Test.HUnit
import Data.List
import PriorityGraph

tests = TestList [
  setsParentToRootTask,
  addsRootParentToNonRootTask,
  addParentWorksOnRootTaskDiscardingOriginalPriority,
  effectivePriorityOfRootTaskIsItsPriority,
  effectivePriorityOfChildTask,
  mapsOverRoot,
  mapsOverTaskWithSingleParent,
  mapsOverTasksWithMultipleParents,
  mapsOverNestedParenting,
  getsTaskListFromTree
                 ]


rootA5 = Root "A" 5
rootB1 = Root "B" 1
childCA3 = Node "C" [(rootA5, 3)]
childDC7B1 = Node "D" [(childCA3, 7), (rootB1, 1)]


setsParentToRootTask =
  setParent (Root "child" 5) (Root "parent" 1) ~?=
    Node "child" [(Root "parent" 1, 5)]

addsRootParentToNonRootTask = 
  addParent childCA3 (rootB1, 2) ~?=
    Node "C" [(rootB1, 2), (rootA5, 3)]

addParentWorksOnRootTaskDiscardingOriginalPriority =
  addParent rootA5 (rootB1, 7) ~?=
    Node "A" [(rootB1, 7)]

effectivePriorityOfRootTaskIsItsPriority =
  priority rootA5 ~?= 5

effectivePriorityOfChildTask = TestList [
  priority (Node "test" []) ~?= 0,
  priority childCA3 ~?= 15,
  priority (addParent rootB1 (childCA3, 2)) ~?= 30,
  priority (addParent childCA3 (rootB1, 2)) ~?= 17
                                        ]

mapsOverRoot =
  fmap (" "++) rootA5 ~?= Root " A" 5

mapsOverTaskWithSingleParent =
  fmap (" "++) childCA3 ~?= Node " C" [(Root " A" 5, 3)]

mapsOverTasksWithMultipleParents =
  (fmap (" "++) $ addParent childCA3 (rootB1, 2)) ~?=
    Node " C" [(Root " B" 1, 2), (Root " A" 5, 3)]

mapsOverNestedParenting =
  fmap (" "++) childDC7B1 ~?=
    Node " D" [
                (Node " C" [(Root " A" 5, 3)], 7),
                (Root " B" 1, 1)
              ]

getsTaskListFromTree =
  getTasks childDC7B1 \\ ["D", "C", "B", "A"] ~?= []

main = runTestTT tests
