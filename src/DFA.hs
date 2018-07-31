module DFA where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import NFA (NfaState, NFA(..))

type DfaState = Set.Set NfaState

data DFA = DFA
  { dfaFinal :: Set.Set DfaState
  , dfaTransitions :: Set.Set (Char, DfaState, DfaState)
  }
  deriving Show

-- | Build a DFA from an NFA
nfaToDfa :: NFA -> DFA
nfaToDfa nfa =
  let
    transitions = expand Set.empty (Set.singleton $ Set.singleton 0) Set.empty
    all_states = concat [ [s1,s2] | (_, s1, s2) <- Set.toList transitions ]
  in
    DFA
      { dfaTransitions = transitions
      , dfaFinal = Set.fromList $ filter (Set.member $ nfaFinal nfa) all_states
      }
  where
    nfa_by_state :: Map.Map NfaState [(Char, NfaState)]
    nfa_by_state = Map.fromListWith (++)
      [(s0, [(c, s1)]) | (c, s0, s1) <- nfaTransitions nfa]

    follow :: DfaState -> [(Char, DfaState, DfaState)]
    follow st =
      (map (\((c,st0),st1) -> (c,st0,st1)) . Map.toList . Map.fromListWith (Set.union))
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
        transitions1 = Set.fromList $ concatMap follow (Set.toList current)

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
