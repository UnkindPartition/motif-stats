module NFA where

import IUPAC (primitiveCodes)

-- | The NFA state is simply the index in the motif.
--
-- 0 is the start state.
type NfaState = Int

data NFA = NFA
  { nfaFinal :: NfaState
  , nfaTransitions :: [(Char, NfaState, NfaState)]
  }
  deriving Show

follow :: [[Char]] -> [(Char, NfaState, NfaState)]
follow = concat . zipWith follow1 [0..]
  where
    follow1
      :: NfaState
      -> [Char]
      -> [(Char, NfaState, NfaState)]
    follow1 n next =
      [ (c, n, n+1) | c <- next ]

-- | Construct an NFA that recognizes the motif preceeded and followed by any number of
-- characters
motifToNFA :: [[Char]] -> NFA
motifToNFA motif =
  NFA
    { nfaFinal = n
    , nfaTransitions = follow motif ++ [(c, 0, 0) | c <- primitiveCodes] ++
        -- once we are in the accepting state, we may stay there
        [(c, n, n) | c <- primitiveCodes]
    }
  where n = length motif
