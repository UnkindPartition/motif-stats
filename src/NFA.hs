module NFA where

import Control.Monad.State
import qualified Data.Map as Map
import IUPAC (primitiveCodes)

-- | The NFA state is simply the index in the motif.
--
-- 0 is the start state.
type NfaState = Int

data NFA = NFA
  { nfaFinal :: [NfaState]
  , nfaTransitions :: [(Char, NfaState, NfaState)]
  }
  deriving Show

type Numbered = [[(NfaState, Char)]]

numberSymbols :: [[Char]] -> Numbered
numberSymbols = zipWith (map . (,)) [1..]

follow :: Numbered -> [(Char, NfaState, NfaState)]
follow motif = concat $ zipWith follow1 motif (tail motif)
  where
    follow1
      :: [(NfaState, Char)]
      -> [(NfaState, Char)]
      -> [(Char, NfaState, NfaState)]
    follow1 this next = do
      (this_state, _) <- this
      (next_state, next_char) <- next
      return (next_char, this_state, next_state)

final :: Numbered -> [NfaState]
final motif
  | null motif = [0]
  | otherwise = fst <$> last motif

-- | Construct an NFA that recognizes the motif preceeded by any number of
-- characters
motifToNFA :: [[Char]] -> NFA
motifToNFA motif =
  let
    numbered = numberSymbols motif
  in
    NFA
      { nfaFinal = final numbered
      , nfaTransitions = follow numbered ++ [(c, 0, 0) | c <- primitiveCodes]
      }
