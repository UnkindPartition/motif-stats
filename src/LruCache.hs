module LruCache where

import Control.Monad.State
import Data.Cache.LRU as LRU
import Data.Tuple
import Data.Bits

class Monad m => MonadLru k v m | m -> k, m -> v where
  getCached :: k -> m v -> m v

newtype LruT k v m a = LruT { unLruT :: StateT (LRU k v) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

runLruT :: (Ord k, Monad m) => LruT k v m a -> Integer -> m a
runLruT a limit =
  let lru0 = newLRU (Just limit)
  in evalStateT (unLruT a) lru0

instance (Ord k, Monad m) => MonadLru k v (LruT k v m) where
  getCached k mv = LruT $ do
    mb_val <- state (swap . LRU.lookup k)
    case mb_val of
      Just v -> return v
      Nothing -> do
        v <- unLruT mv
        modify' (LRU.insert k v)
        return v

-- | LRU-based power. The same caching monad context should only be used for
-- a single value of x.
lruPow :: forall s m . (Monoid s, MonadLru Int s m) => Int -> s -> m s
lruPow n x
  | n < 0 = error "lruPow: negative power"
  | n == 0 = return mempty -- this might be a problem in hmatrix since mempty is 1x1
  | n == 1 = return x
  | otherwise = getCached n notCached
  where
    notCached :: m s
    notCached = do
      let
        pow2 = maxPow2 n
        r = n - pow2
      if r == 0
        then do
          let pow2_half = pow2 `quot` 2
          liftM (\y -> y <> y) (lruPow pow2_half x)
        else
          liftM2 (<>) (lruPow pow2 x) (lruPow r x)

-- | Highest power of 2 that is ≤ n
maxPow2 :: Int -> Int
maxPow2 n = shiftL 1 (finiteBitSize n - 1 - countLeadingZeros n)
