module Partition where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable
import Control.Monad.State
import Utils

-- | A partition of values into disjoint groups. This data structure allows
-- us to lookup the group by state and states by a group, and to implement
-- some useful generic algorithms that preserve the invariant.
data Partition s = Partition (Map.Map s Int) (Map.Map Int (Set.Set s))

-- | This instance assumes that partitions are disjoint
instance Ord s => Semigroup (Partition s) where
  Partition s2i_1 i2s_1 <> Partition s2i_2 i2s_2 =
    Partition (s2i_1 <> s2i_2) (i2s_1 <> i2s_2)
instance Ord s => Monoid (Partition s) where
  mappend = (<>)
  mempty = Partition mempty mempty

-- | Create a partition consisting of a single group
singleton
  :: (Ord s, MonadState Int m)
  => Set.Set s
  -> m (Partition s)
singleton s = do
  i <- nextInt
  return $ Partition
    (Map.fromAscList [(s1,i) | s1 <- Set.toAscList s])
    (Map.singleton i s)

-- | Number of groups in a partition
size :: Partition s -> Int
size (Partition _ i2s) = Map.size i2s

lookupState :: Ord s => Partition s -> s -> Int
lookupState (Partition s2i _) s = s2i Map.! s

-- | Given a classification function, partition a given set such
-- that every group in the partition belong to the same class according
-- to the classification function.
partitionSet
  :: forall s z m . (Ord s, Ord z, MonadState Int m)
  => (s -> z)
  -> Set.Set s
  -> m (Partition s)
partitionSet fn s = do
  let
    z_values :: [(s, z)]
    z_values = [ (s1, fn s1) | s1 <- Set.toAscList s ]

  z2i :: Map.Map z Int
    <- fmap Map.fromList . sequence $
      -- the list will have repeated z values; Map.fromList will retain the
      -- last one
      [ (,) z <$> nextInt | (_, z) <- z_values ]

  let
    i_values :: [(s, Int)]
    i_values = [ (s1, z2i Map.! z) | (s1, z) <- z_values ]

  return $ Partition
    (Map.fromAscList i_values)
    (Map.fromListWith Set.union [(i,Set.singleton s1) | (s1,i) <- i_values])

-- | Given a classification function, subpartition a given partition such
-- that every group in the new partition belong to the same class according
-- to the classification function.
subpartition
  :: (Ord s, Ord z, MonadState Int m)
  => (s -> z)
  -> Partition s
  -> m (Partition s)
subpartition fn (Partition _ i2s) =
  let
    groups0 = Map.elems i2s
  in fmap fold . mapM (partitionSet fn) $ groups0

-- | Given a classification function, subpartition a given partition such
-- that every group in the new partition belong to the same class according
-- to the classification function. Then do the same to the new partition
-- and so on, until the process converges.
--
-- Unlike 'subpartition', the function has access to the current partition,
-- because it changes from iteration to iteration.
subpartitionFixpoint
  :: (Ord s, Ord z, MonadState Int m)
  => (Partition s -> s -> z)
  -> Partition s
  -> m (Partition s)
subpartitionFixpoint fn = fix $ \rec pt0 -> do
  pt1 <- subpartition (fn pt0) pt0
  if size pt1 == size pt0
    then return pt1
    else rec pt1
