import Test.HUnit
import PriorityGraph

tests = TestList [
  addsParentToTask
                 ]


addsParentToTask =
  addParent (Root ("child", 5)) (Root ("parent", 1)) ~?=
    PriorityGraph ("child", [(Root ("parent", 1), 5)])

main = runTestTT tests
