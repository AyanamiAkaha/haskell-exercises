import Test.HUnit
import PriorityGraph

tests = TestList [
  setsParentToRootTask,
  addsRootParentToNonRootTask,
  addParentWorksOnRootTaskDiscartingOriginalPriority
                 ]


setsParentToRootTask =
  setParent (Root ("child", 5)) (Root ("parent", 1)) ~?=
    PriorityGraph ("child", [(Root ("parent", 1), 5)])

rootA5 = Root ("A", 5)
rootB1 = Root ("B", 1)
childCA3 = PriorityGraph ("C", [(rootA5, 3)])

addsRootParentToNonRootTask = 
  addParent childCA3 (rootB1, 2) ~?=
    PriorityGraph ("C", [(rootB1, 2), (rootA5, 3)])

addParentWorksOnRootTaskDiscartingOriginalPriority =
  addParent rootA5 (rootB1, 7) ~?=
    PriorityGraph ("A", [(rootB1, 7)])


main = runTestTT tests
