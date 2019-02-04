import Test.HUnit
import RPN

tests = TestList [
          returnsRPNFromValue
--          performsCalculation
                 ]

-- performsCalculation = TestCase (return 1 >>= push 1 >>= push add) ~?= return 3

returnsRPNFromValue = (
  (return 1 :: RPN Int) ~?= RPN [Number 1]
                      )

main = runTestTT tests
