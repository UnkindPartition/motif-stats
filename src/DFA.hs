{-# LANGUAGE RecordWildCards #-}
module DFA where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.Maybe
import Data.Semigroup (stimes)
import Numeric.LinearAlgebra hiding (remap, (<>))
import NFA
import Utils
import qualified Partition as P
import LruCache

-- | The DFA type is parameterized on the state type, so that we can use
-- Sets during the subset construction and then map them to integers for
-- better efficiency.
data DFA s c = DFA
  { dfaStart :: s
  , dfaFinal :: Set.Set s
  , dfaStates :: Set.Set s
  , dfaTransitions :: Set.Set (c, s, s)
  }
  deriving Show

-- | At first, the DFA states are sets of NFA states
type DfaState1 = Set.Set NfaState

-- | Then, the sets are mapped into integers
type DfaState2 = Int

----------------------------------------------------------------------
--                      The subset construction
----------------------------------------------------------------------

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
move :: Ord c => NFA (Maybe c) -> DfaState1 -> c -> DfaState1
move NFA{nfaTransitions} ss c = sconcatMap
  (\s -> fromMaybe mempty . Map.lookup (Just c) .
    fromMaybe mempty . Map.lookup s $
    transitionMap nfaTransitions)
  ss

-- | Build a DFA from an NFA
nfaToDfa :: forall c . Ord c => [c] -> NFA (Maybe c) -> DFA DfaState1 c
nfaToDfa alphabet nfa =
  let
    start :: DfaState1
    start = epsilonClosure nfa (nfaStart nfa)
    transitions = go Set.empty (Set.singleton $ start) Set.empty
    all_states = concat [ [s1,s2] | (_, s1, s2) <- Set.toList transitions ]
    contains_final :: DfaState1 -> Bool
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
      :: Set.Set DfaState1
      -> Set.Set DfaState1
      -> Set.Set (c, DfaState1, DfaState1)
      -> Set.Set (c, DfaState1, DfaState1)
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
            next :: DfaState1
            next = epsilonClosure nfa (move nfa s c)
          in (Set.singleton (c, s, next), Set.singleton next)

        transitions2 = transitions0 `Set.union` transitions1
        new2 = new1 `Set.difference` seen1
      in go seen1 new2 transitions2

----------------------------------------------------------------------
--                         DFA optimization
----------------------------------------------------------------------

-- | Map all DFA states
mapStates
  :: (Ord s1, Ord s2, Ord c)
  => (s1 -> s2)
  -> DFA s1 c
  -> DFA s2 c
mapStates remap DFA{..} =
  DFA
  { dfaStart = remap dfaStart
  , dfaFinal = Set.map remap dfaFinal
  , dfaStates = Set.map remap dfaStates
  , dfaTransitions = Set.map (\(c,s0,s1) -> (c, remap s0, remap s1)) dfaTransitions
  }

-- | Convert all states of the DFA to consecutive integers starting from 0
mapStatesToInt
  :: (Ord s, Ord c)
  => DFA s c
  -> DFA DfaState2 c
mapStatesToInt dfa =
  let
    stateMap = Map.fromList $ zip (Set.toList . dfaStates $ dfa) [0..]
    remap = (stateMap Map.!)
  in
    mapStates remap dfa

minimizeDfa
  :: forall s c . (Ord c, Ord s)
  => DFA s c
  -> DFA Int c
minimizeDfa dfa@DFA{..} = flip evalState 0 $ do
  initialPartition <- liftM2 (<>)
    (P.singleton dfaFinal)
    (P.singleton $ dfaStates `Set.difference` dfaFinal)

  let
    transitionMap :: Map.Map s (Map.Map c s)
    transitionMap = Map.fromListWith Map.union
      [ (s0, Map.singleton c s1)
      | (c, s0, s1) <- Set.toList dfaTransitions
      ]

    classify :: P.Partition s -> s -> Map.Map c Int
    classify pt s = fmap (P.lookupState pt) $ transitionMap Map.! s

  finalPartition <- P.subpartitionFixpoint classify initialPartition

  return $ mapStates (P.lookupState finalPartition) dfa

----------------------------------------------------------------------
--                Transfer matrices and probabilities
----------------------------------------------------------------------

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
  :: (Ord c, Ord s)
  => DFA s c
  -> TransferMatrix
dfaTransferMatrix (mapStatesToInt -> DFA{..}) =
  -- we remap the DFA once again to make sure the numbers are consecutive
  let
    n = Set.size dfaStates
    elts =
      [ ((s0, s1), 0.25)
      | (_, s0, s1) <- Set.toList dfaTransitions
      ]
  in
    TransferMatrix
    { tmMatrix = (assoc (n,n) 0 . assocSum) elts
    , tmStart = assoc n 0 [ (dfaStart, 1) ]
    , tmFinal = assoc n 0 [ (st, 1) | st <- Set.toList dfaFinal ]
    }
  where
    assocSum = Map.toList . Map.fromListWith (+)

dfaProbability
  :: TransferMatrix
  -> Int -- ^ length of the random string where they motif may occur
  -> Double
dfaProbability TransferMatrix{..} len =
  (tmStart <# stimes len tmMatrix) <.> tmFinal

dfaProbabilityCached
  :: MonadLru Int (Matrix Double) m
  => TransferMatrix
  -> Int -- ^ length of the random string where they motif may occur
  -> m Double
dfaProbabilityCached TransferMatrix{..} len = do
  pw <- lruPow len tmMatrix
  return $ (tmStart <# pw) <.> tmFinal
