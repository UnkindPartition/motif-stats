module DFA where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import NFA (NfaState, NFA(..))

type DfaState = Set.Set NfaState

data DFA sym state = DFA
  { dfaStart :: state
  , dfaFinal :: Set.Set state
  , dfaStates :: Set.Set state
  , dfaTransitions :: Set.Set (sym, state, state)
  }
  deriving Show

-- | Build a DFA from an NFA
nfaToDfa :: NFA -> DFA Char DfaState
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
      (map (\((c,st0),st1) -> (c,st0,st1)) . Map.toList . Map.fromListWith Set.union)
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

data MarkedSym sym
  = Sym sym
    -- ^ a normal symbol inherited from the non-marked DFA
  | Mark
    -- ^ the mark, which follows a match of the regex
  deriving (Eq, Ord, Show)

data MarkedState state
  = NormalState state
    -- ^ a normal state inherited from the non-marked DFA
  | MarkedState state
    -- ^ a marked state, which, after recognizing 'Mark', leads to @state@
  deriving (Eq, Ord, Show)

-- | Produce a «marked» automaton
markDfa
  :: (Ord sym, Ord state)
  => DFA sym state
  -> DFA (MarkedSym sym) (MarkedState state)
markDfa dfa0 = DFA
  { dfaStart = NormalState $ dfaStart dfa0
  , dfaFinal = Set.map NormalState (dfaStates dfa0)
  , dfaStates = Set.union finalStates markedStates
  , dfaTransitions = Set.fromList $ do
      (c, s0, s1) <- Set.toList $ dfaTransitions dfa0
      if Set.member s1 (dfaFinal dfa0)
        then
          [ (Sym c, NormalState s0, MarkedState s1)
          , (Mark,  MarkedState s1, NormalState s1)
          ]
        else return (Sym c, NormalState s0, NormalState s1)
  }
  where
    finalStates = Set.map NormalState (dfaStates dfa0)
    markedStates = Set.map MarkedState (dfaFinal dfa0)
