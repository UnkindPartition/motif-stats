{-# LANGUAGE RecordWildCards #-}
module DFA where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapStrict
import Data.Maybe
import Data.Semigroup (stimes)
import Numeric.LinearAlgebra
import NFA

type DfaState = Set.Set NfaState

data DFA c = DFA
  { dfaStart :: DfaState
  , dfaFinal :: Set.Set DfaState
  , dfaStates :: Set.Set DfaState
  , dfaTransitions :: Set.Set (c, DfaState, DfaState)
  }
  deriving Show

-- | Like 'concatMap', but for 'Set.Set's
sconcatMap :: (Ord a, Ord b) => (a -> Set.Set b) -> Set.Set a -> Set.Set b
sconcatMap f = mconcat . map f . Set.toList

-- | Return all epsilon-edges from an NFA state
epsilonEdges :: Ord c => NFA (Maybe c) -> NfaState -> Set.Set NfaState
epsilonEdges NFA{nfaTransitions} s =
  fromMaybe mempty . Map.lookup Nothing . fromMaybe mempty . Map.lookup s $
    transitionMap nfaTransitions

-- | Compute an epsilon-closure
epsilonClosure :: Ord c => NFA (Maybe c) -> Set.Set NfaState -> Set.Set NfaState
epsilonClosure nfa = go mempty
  where
    go !seen0 !new0
      | Set.null new0 = seen0
      | otherwise =
      let
        seen1 = Set.union seen0 new0
        new1 = Set.difference (sconcatMap (epsilonEdges nfa) new0) seen0
      in go seen1 new1

-- | @move nfa s a@ is a set of NFA states where we can get from one of the
-- states in @s@ after receiving @a@ as an input
move :: Ord c => NFA (Maybe c) -> DfaState -> c -> DfaState
move NFA{nfaTransitions} ss c = sconcatMap
  (\s -> fromMaybe mempty . Map.lookup (Just c) .
    fromMaybe mempty . Map.lookup s $
    transitionMap nfaTransitions)
  ss

-- | Build a DFA from an NFA
nfaToDfa :: forall c . Ord c => [c] -> NFA (Maybe c) -> DFA c
nfaToDfa alphabet nfa =
  let
    start :: DfaState
    start = epsilonClosure nfa (Set.singleton $ nfaStart nfa)
    transitions = go Set.empty (Set.singleton $ start) Set.empty
    all_states = concat [ [s1,s2] | (_, s1, s2) <- Set.toList transitions ]
    contains_final :: DfaState -> Bool
    contains_final dfa_st = not . Set.null $ Set.intersection (nfaFinal nfa) dfa_st
  in
    DFA
      { dfaStart = start
      , dfaFinal = Set.fromList $ filter contains_final all_states
      , dfaStates = Set.fromList all_states
      , dfaTransitions = transitions
      }
  where
    go
      :: Set.Set DfaState
      -> Set.Set DfaState
      -> Set.Set (c, DfaState, DfaState)
      -> Set.Set (c, DfaState, DfaState)
    go !seen0 !new0 !transitions0
      | Set.null new0 = transitions0
      | otherwise =
      let
        seen1 = Set.union seen0 new0
        -- where can we get from the DFA states in new1?
        (transitions1, new1) =
          flip foldMap new0 $ \s ->
          flip foldMap alphabet $ \c ->
          let
            next :: DfaState
            next = epsilonClosure nfa (move nfa s c)
          in (Set.singleton (c, s, next), Set.singleton next)

        transitions2 = transitions0 `Set.union` transitions1
        new2 = new1 `Set.difference` seen1
      in go seen1 new2 transitions2

data TransferMatrix = TransferMatrix
  { tmMatrix :: Matrix Double
    -- ^ transfer matrix itself
  , tmStart :: Vector Double
    -- ^ the indicator vector of the start state
  , tmFinal :: Vector Double
    -- ^ the indicator vector of the final states
  }
  deriving (Show)

-- | Construct the DFA transfer matrix assuming the uniform distribution
-- over nucleotides.
--
-- Return the matrix and the 0-based indices of the start and end states.
dfaTransferMatrix
  :: DFA c
  -> TransferMatrix
dfaTransferMatrix DFA{..} =
  let
    stateToInt = Map.fromList $ zip (Set.toList dfaStates) [0..]
    n = Map.size stateToInt
    elts =
      [ ((stateToInt Map.! s0, stateToInt Map.! s1), 0.25)
      | (_, s0, s1) <- Set.toList dfaTransitions
      ]
  in
    TransferMatrix
    { tmMatrix = (assoc (n,n) 0 . assocSum) elts
    , tmStart = assoc n 0 [ (stateToInt Map.! dfaStart, 1) ]
    , tmFinal = assoc n 0 [ (stateToInt Map.! st, 1) | st <- Set.toList dfaFinal ]
    }
  where
    assocSum = Map.toList . MapStrict.fromListWith (+)

dfaProbability
  :: TransferMatrix
  -> Int -- ^ length of the random string where they motif may occur
  -> Double
dfaProbability TransferMatrix{..} len =
  (tmStart <# stimes len tmMatrix) <.> tmFinal
