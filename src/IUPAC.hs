module IUPAC where

import Data.Char
import qualified Data.Map as Map
import NFA

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
  ] ++ [(c,[c]) | c <- primitiveCodes]

expandIUPAC :: [Char] -> [[Char]]
expandIUPAC = map $ \c ->
  case Map.lookup (toUpper c) iupacCodes of
    Just cs -> cs
    Nothing -> error $ "Unknown IUPAC code: " ++ show c

primitiveCodes :: [Char]
primitiveCodes = "ACGT"

-- | Whether to count the reverse complement
data RC = RC | NoRC

comp :: Char -> Char
comp 'A' = 'T'
comp 'T' = 'A'
comp 'G' = 'C'
comp 'C' = 'G'
comp c = error $ "comp: unknown symbol: " ++ show c

-- | Construct an NFA that recognizes the motif preceeded and followed by any number of
-- characters
motifToNFA :: RC -> String -> NFA (Maybe Char)
motifToNFA rc motif =
  let
    expanded = expandIUPAC motif
    expanded_rc = reverse $ map (map comp) expanded
    mk_re cs = foldr Seq Empty $ map (foldr1 Alt . map Sym) cs
    re =
      case rc of
        RC -> mk_re expanded `Alt` mk_re expanded_rc
        NoRC -> mk_re expanded
  in compile primitiveCodes $ Star AnySym `Seq` re `Seq` Star AnySym
