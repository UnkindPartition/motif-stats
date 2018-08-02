module NFA where

import qualified Data.Set as Set
import IUPAC (primitiveCodes)

newtype NfaState = NfaState Int
  deriving (Eq, Ord, Show)

data NFA = NFA
  { nfaStart :: NfaState
  , nfaFinal :: Set.Set NfaState
  , nfaTransitions :: [(Char, NfaState, NfaState)]
  }
  deriving Show

follow :: [[Char]] -> [(Char, NfaState, NfaState)]
follow = concat . zipWith follow1 [0..]
  where
    follow1
      :: Int
      -> [Char]
      -> [(Char, NfaState, NfaState)]
    follow1 n next =
      [ (c, NfaState n, NfaState (n+1)) | c <- next ]

-- | Construct an NFA that recognizes the motif preceeded and followed by any number of
-- characters
motifToNFA :: [[Char]] -> NFA
motifToNFA motif =
  NFA
    { nfaStart = start
    , nfaFinal = Set.singleton (NfaState n)
    , nfaTransitions = follow motif ++ [(c, start, start) | c <- primitiveCodes] ++
        -- once we are in the accepting state, we may stay there
        [(c, end, end) | c <- primitiveCodes]
    }
  where
    n = length motif
    start = NfaState 0
    end = NfaState n
