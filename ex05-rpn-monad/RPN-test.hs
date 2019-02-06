import Test.HUnit
import Token
import RPN

tests = TestList [
  tokenCanBeParsed,
  stringCanBeParsedIntoCalc
--          returnsRPNFromValue
--          performsCalculation
                 ]

tokenCanBeParsed = TestList [
  (tparse "1") ~?= tnum 1,
  (tparse "+") ~?= Op Add,
  (tparse "-") ~?= Op Sub,
  (tparse "*") ~?= Op Mul,
  (tparse "/") ~?= Op Div,
  (tparse "asdf") ~?= BadInput
                            ]
  
stringCanBeParsedIntoCalc = TestList [
  (rpn "1") ~?= RPN [tnum 1],
  (rpn "+") ~?= RPN [Op Add],
  (rpn "-") ~?= RPN [Op Sub],
  (rpn "*") ~?= RPN [Op Mul],
  (rpn "/") ~?= RPN [Op Div],
  (rpn "1 2") ~?= RPN [tnum 1, tnum 2],
  (rpn "x") ~?= RPN [BadInput]
                                     ]

-- performsCalculation = TestCase (return 1 >>= push 1 >>= push add) ~?= return 3

-- returnsRPNFromValue = (
--   (return 1 :: RPN Int) ~?= RPN [Number 1]
--                       )

main = runTestTT tests
