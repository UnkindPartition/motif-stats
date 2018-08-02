module NFA
  ( NfaState(..)
  , NfaTransitions(..)
  , NFA(..)
  , motifToNFA
  )
  where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import IUPAC (primitiveCodes)

newtype NfaState = NfaState Int
  deriving (Eq, Ord, Show)

newtype NfaTransitions c = NfaTransitions
  { transitionMap :: Map.Map NfaState (Map.Map c (Set.Set NfaState)) }
  deriving (Eq, Ord, Show)

instance Ord c => Semigroup (NfaTransitions c) where
  NfaTransitions a <> NfaTransitions b = NfaTransitions $ Map.unionWith (Map.unionWith mappend) a b
instance Ord c => Monoid (NfaTransitions c) where
  mappend = (<>)
  mempty = NfaTransitions mempty

transitionsFromList :: Ord c => [(c, NfaState, NfaState)] -> NfaTransitions c
transitionsFromList l = NfaTransitions $ Map.fromListWith (Map.unionWith mappend)
  [ (s0, Map.singleton c (Set.singleton s1)) | (c, s0, s1) <- l ]

data NFA c = NFA
  { nfaStart :: Set.Set NfaState
  , nfaFinal :: Set.Set NfaState
  , nfaTransitions :: NfaTransitions c
  }
  deriving Show

follow :: Ord c => [[c]] -> NfaTransitions (Maybe c)
follow = transitionsFromList . concat . zipWith follow1 [0..]
  where
    follow1
      :: Int
      -> [c]
      -> [(Maybe c, NfaState, NfaState)]
    follow1 n next =
      [ (Just c, NfaState n, NfaState (n+1)) | c <- next ]

-- | Construct an NFA that recognizes the motif preceeded and followed by any number of
-- characters
motifToNFA :: [[Char]] -> NFA (Maybe Char)
motifToNFA motif =
  NFA
    { nfaStart = Set.singleton start
    , nfaFinal = Set.singleton (NfaState n)
    , nfaTransitions =
        follow motif <>
        transitionsFromList [(Just c, start, start) | c <- primitiveCodes] <>
        -- once we are in the accepting state, we may stay there
        transitionsFromList [(Just c, end, end) | c <- primitiveCodes]
    }
  where
    n = length motif
    start = NfaState 0
    end = NfaState n
