module IUPAC where

import Data.Char
import qualified Data.Map as Map
import Text.ParserCombinators.ReadP
import NFA
import DFA

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

-- | Convert an IUPAC motif to a transfer matrix
motifToTm
  :: RC
  -> Map.Map Char Double
  -> String
  -> TransferMatrix
motifToTm rc freqs
  = dfaTransferMatrix freqs
  . minimizeDfa . mapStatesToInt
  . nfaToDfa primitiveCodes . motifToNFA rc

parseFreqs :: String -> Either String [Double]
parseFreqs str = checkLength =<< runParser (sepBy dbl (char ',') <* eof)
  where
    dbl :: ReadP Double
    dbl = readS_to_P reads

    runParser :: ReadP a -> Either String a
    runParser r =
      case readP_to_S r str of
        [(v, "")] -> Right v
        _ -> Left $ "Cannot parse frequencies from " ++ show str

    checkLength :: [Double] -> Either String [Double]
    checkLength l | length l == 4 = Right l
    checkLength [a,c] = Right [a,c,c,a]
    checkLength l = Left $ "2 or 4 frequencies required, got " ++ show (length l)
