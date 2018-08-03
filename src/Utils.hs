module Utils where

import Control.Monad.State
import qualified Data.Set as Set

-- | Generate a fresh integer
nextInt :: MonadState Int m => m Int
nextInt = do
  n <- get
  put $! n+1
  return n

-- | Like 'concatMap', but for 'Set.Set's
sconcatMap :: (Ord a, Ord b) => (a -> Set.Set b) -> Set.Set a -> Set.Set b
sconcatMap f = mconcat . map f . Set.toList
