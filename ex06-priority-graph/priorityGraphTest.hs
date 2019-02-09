import Test.HUnit
import PriorityGraph

tests = TestList [
  setsParentToRootTask,
  addsRootParentToNonRootTask,
  addParentWorksOnRootTaskDiscardingOriginalPriority,
  effectivePriorityOfRootTaskIsItsPriority,
  effectivePriorityOfChildTask
                 ]


rootA5 = Root ("A", 5)
rootB1 = Root ("B", 1)
childCA3 = PriorityGraph ("C", [(rootA5, 3)])


setsParentToRootTask =
  setParent (Root ("child", 5)) (Root ("parent", 1)) ~?=
    PriorityGraph ("child", [(Root ("parent", 1), 5)])

addsRootParentToNonRootTask = 
  addParent childCA3 (rootB1, 2) ~?=
    PriorityGraph ("C", [(rootB1, 2), (rootA5, 3)])

addParentWorksOnRootTaskDiscardingOriginalPriority =
  addParent rootA5 (rootB1, 7) ~?=
    PriorityGraph ("A", [(rootB1, 7)])

effectivePriorityOfRootTaskIsItsPriority =
  priority rootA5 ~?= 5

effectivePriorityOfChildTask = TestList [
  priority (PriorityGraph ("test", [])) ~?= 0,
  priority childCA3 ~?= 15,
  priority (addParent rootB1 (childCA3, 2)) ~?= 30,
  priority (addParent childCA3 (rootB1, 2)) ~?= 17
                                        ]

main = runTestTT tests
