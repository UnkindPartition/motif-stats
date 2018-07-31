module NFA where

import Control.Monad.State
import qualified Data.Map as Map

data NfaState
  = NfaStart
  | NfaState !Int
  deriving (Eq, Ord, Show)

data NFA = NFA
  { nfaFinal :: [NfaState]
  , nfaTransitions :: [(Char, NfaState, NfaState)]
  }
  deriving Show

type Numbered = [[(NfaState, Char)]]

numberSymbols :: [[Char]] -> Numbered
numberSymbols = flip evalState 0 . traverse (traverse number1)
  where
    number1 :: b -> State Int (NfaState, b)
    number1 b = do
      i <- get
      put $! i+1
      return (NfaState i, b)

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
  | null motif = [NfaStart]
  | otherwise = fst <$> last motif

motifToNFA :: [[Char]] -> NFA
motifToNFA motif =
  let
    numbered = numberSymbols motif
  in
    NFA
      { nfaFinal = final numbered
      , nfaTransitions = follow numbered
      }
