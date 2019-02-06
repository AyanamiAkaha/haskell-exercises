import Test.HUnit
import Token
import RPN

tests = TestList [
  tokenCanBeParsed,
  stringCanBeParsedIntoCalc,
  returnsRPNFromValue
--  calculatesValueWhileParsing 
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


returnsRPNFromValue = (
  (return 1 :: RPN Int) ~?= RPN [tnum 1]
                      )

-- calculatesValueWhileParsing = TestList [
--   (rpn "1 2 +") ~?= RPN [tnum 3],
--   (rpn "1 2 -") ~?= RPN [tnum (-1)],
--   (rpn "1 2 *") ~?= RPN [tnum 2],
--   (rpn "1 2 /") ~?= RPN [tnum 0.5],
--   (rpn "1 2 3 * +") ~?= RPN [tnum 7],
--   (rpn "1 2 3 +") ~?= RPN [tnum 1, tnum 5],
--   (rpn "2 2 * +") ~?= RPN [tnum 4, Op Add]
--                                        ]

-- performsCalculation = TestCase (return 1 >>= push 1 >>= push add) ~?= return 3

main = runTestTT tests
