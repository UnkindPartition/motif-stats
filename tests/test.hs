import Test.Tasty
import Test.Tasty.HUnit
import DFA
import NFA
import IUPAC

-- | Calculate a probability for one strand only
iupacProb1 :: String -> Int -> Double
iupacProb1 motif =
  dfaProbability . dfaTransferMatrix . nfaToDfa . motifToNFA . expandIUPAC $ motif

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "\"C\", 1 strand, n = 1" $
      iupacProb1 "C" 1 @?= 0.25
  , testCase "\"W\", 1 strand, n = 1" $
      iupacProb1 "W" 1 @?= 0.5
  , testCase "\"CCC\", 1 strand, n = 3" $
      iupacProb1 "CCC" 3 @?= 0.25^3
  , testCase "\"TT\", 1 strand, n = 3" $
      iupacProb1 "TT" 3 @?= 3/4 * 1/16 + 1/16
  , testCase "\"TA\", 1 strand, n = 3" $
      iupacProb1 "TA" 3 @?= 1/4 * (1/4 + 1/16) + 3/4 * 1/16
  ]
