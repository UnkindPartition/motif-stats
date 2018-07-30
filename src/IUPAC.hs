module IUPAC where

import Data.Char
import qualified Data.Map as Map

iupacCodes :: Map.Map Char [Char]
iupacCodes = Map.fromList $
  [ ('R', "AG")
  , ('Y', "CT")
  , ('S', "GC")
  , ('W', "AT")
  , ('K', "GT")
  , ('M', "AC")
  , ('B', "CGT")
  , ('D', "AGT")
  , ('H', "ACT")
  , ('V', "ACG")
  , ('N', "ACGT")
  ] ++ [(c,[c]) | c <- "ACGT"]

expandIUPAC :: [Char] -> [[Char]]
expandIUPAC = map $ \c ->
  case Map.lookup (toUpper c) iupacCodes of
    Just cs -> cs
    Nothing -> error $ "Unknown IUPAC code: " ++ show c
