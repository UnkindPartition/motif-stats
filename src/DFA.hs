{-# LANGUAGE RecordWildCards #-}
module DFA where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Map.Strict as MapStrict
import Data.Maybe
import Data.Semigroup (stimes)
import Numeric.LinearAlgebra
import NFA (NfaState, NFA(..))

type DfaState = Set.Set NfaState

data DFA = DFA
  { dfaStart :: DfaState
  , dfaFinal :: Set.Set DfaState
  , dfaStates :: Set.Set DfaState
  , dfaTransitions :: Set.Set (Char, DfaState, DfaState)
  }
  deriving Show

-- | Build a DFA from an NFA
nfaToDfa :: NFA -> DFA
nfaToDfa nfa =
  let
    start :: DfaState
    start = Set.singleton 0
    transitions = expand Set.empty (Set.singleton start) Set.empty
    all_states = concat [ [s1,s2] | (_, s1, s2) <- Set.toList transitions ]
  in
    DFA
      { dfaStart = start
      , dfaFinal = Set.fromList $ filter (Set.member $ nfaFinal nfa) all_states
      , dfaStates = Set.fromList all_states
      , dfaTransitions = transitions
      }
  where
    nfa_by_state :: Map.Map NfaState [(Char, NfaState)]
    nfa_by_state = Map.fromListWith (++)
      [(s0, [(c, s1)]) | (c, s0, s1) <- nfaTransitions nfa]

    follow :: DfaState -> [(Char, DfaState, DfaState)]
    follow st =
      (map (\((c,st0),st1) -> (c,st0,st1)) . Map.toList . MapStrict.fromListWith Set.union)
      [ ((c, st), Set.singleton s1)
      | s0 <- Set.toList st
      , (c, s1) <- fromMaybe [] $ Map.lookup s0 nfa_by_state
      ]
    -- Expand the set of transitions given a set of starting states
    expand
      :: Set.Set DfaState
      -> Set.Set DfaState
      -> Set.Set (Char, DfaState, DfaState)
      -> Set.Set (Char, DfaState, DfaState)
    expand visited0 current transitions0 =
      let
        transitions1 :: Set.Set (Char, DfaState, DfaState)
        transitions1 =
          Set.union
            (Set.fromList $ concatMap follow (Set.toList current))
            transitions0

        visited1 = Set.union visited0 current
        new_states =
          Set.difference
            (Set.map (\(_,_,s) -> s) transitions1)
            visited1
      in
        if Set.size transitions1 == Set.size transitions0
          then -- no progress was made; closure reached
            transitions1
          else
            expand visited1 new_states transitions1

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
  :: DFA
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
  :: Int -- ^ length of the random string where they motif may occur
  -> TransferMatrix
  -> Double
dfaProbability len TransferMatrix{..} =
  (tmStart <# stimes len tmMatrix) <.> tmFinal
