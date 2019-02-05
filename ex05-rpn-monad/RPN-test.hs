import Test.HUnit
import Token
import RPN

tests = TestList [
  tokenCanBeParsed
--          returnsRPNFromValue
--          performsCalculation
                 ]

tokenCanBeParsed = TestList [
  (tparse "1") ~?= tnum 1,
  (tparse "add") ~?= Op Add,
  (tparse "sub") ~?= Op Sub,
  (tparse "mul") ~?= Op Mul,
  (tparse "div") ~?= Op Div,
  (tparse "asdf") ~?= BadInput
                            ]
  

-- performsCalculation = TestCase (return 1 >>= push 1 >>= push add) ~?= return 3

-- returnsRPNFromValue = (
--   (return 1 :: RPN Int) ~?= RPN [Number 1]
--                       )

main = runTestTT tests
