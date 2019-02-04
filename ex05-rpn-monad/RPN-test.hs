import Test.HUnit
import Token
import RPN

tests = TestList [
  tokenCanBeRead
--          returnsRPNFromValue
--          performsCalculation
                 ]

tokenCanBeRead = TestList [
  (read "1" :: Token) ~?= Num 1,
  (read "add" :: Token) ~?= Op Add,
  (read "sub" :: Token) ~?= Op Sub,
  (read "mul" :: Token) ~?= Op Mul,
  (read "div" :: Token) ~?= Op Div,
  (read "asdf" :: Token) ~?= BadInput
                          ]
  

-- performsCalculation = TestCase (return 1 >>= push 1 >>= push add) ~?= return 3

-- returnsRPNFromValue = (
--   (return 1 :: RPN Int) ~?= RPN [Number 1]
--                       )

main = runTestTT tests
