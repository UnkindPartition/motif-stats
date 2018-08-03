import Test.Tasty
import Test.Tasty.HUnit
import DFA
import IUPAC

-- | Calculate a probability for one strand only
iupacProb1 :: RC -> String -> Int -> Double
iupacProb1 rc motif =
  dfaProbability $ motifToTm rc motif

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "\"C\", 1 strand, n = 1" $
      iupacProb1 NoRC "C" 1 @?= 0.25
  , testCase "\"W\", 1 strand, n = 1" $
      iupacProb1 NoRC "W" 1 @?= 0.5
  , testCase "\"CCC\", 1 strand, n = 3" $
      iupacProb1 NoRC "CCC" 3 @?= 0.25^3
  , testCase "\"TT\", 1 strand, n = 3" $
      iupacProb1 NoRC "TT" 3 @?= 3/4 * 1/16 + 1/16
  , testCase "\"TA\", 1 strand, n = 3" $
      iupacProb1 NoRC "TA" 3 @?= 1/4 * (1/4 + 1/16) + 3/4 * 1/16
  , testCase "\"A\", 2 strands, n = 1" $
      iupacProb1 RC "A" 1 @?= 0.5
  , testCase "\"GC\", 2 strands, n = 2" $
      iupacProb1 RC "GC" 2 @?= iupacProb1 NoRC "GC" 2 -- revcomp of itself
  , testCase "\"GG\", 2 strands, n = 2" $
      iupacProb1 RC "GG" 2 @?= 2 * iupacProb1 NoRC "GG" 2
  , testCase "\"K\", 2 strands, n = 1" $
      iupacProb1 RC "K" 2 @?= 1
  ]
