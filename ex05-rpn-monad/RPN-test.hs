import Test.HUnit
import Token
import RPN

tests = TestList [
  tokenCanBeParsed
--          returnsRPNFromValue
--          performsCalculation
                 ]

tokenCanBeParsed = TestList [
  (parse "1" :: Token) ~?= Num 1,
  (parse "add" :: Token) ~?= Op Add,
  (parse "sub" :: Token) ~?= Op Sub,
  (parse "mul" :: Token) ~?= Op Mul,
  (parse "div" :: Token) ~?= Op Div,
  (parse "asdf" :: Token) ~?= BadInput
                          ]
  

-- performsCalculation = TestCase (return 1 >>= push 1 >>= push add) ~?= return 3

-- returnsRPNFromValue = (
--   (return 1 :: RPN Int) ~?= RPN [Number 1]
--                       )

main = runTestTT tests
