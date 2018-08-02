module NFA
  ( Regex(..)
  , NfaState(..)
  , NfaTransitions(..)
  , NFA(..)
  , compile
  , motifToNFA
  )
  where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.State.Strict
import IUPAC (primitiveCodes)

data Regex c
  = Sym c
  | Seq (Regex c) (Regex c)
  | Alt (Regex c) (Regex c)
  | Plus (Regex c)
  | Star (Regex c)
  | Empty

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

compile :: Ord c => Regex c -> NFA (Maybe c)
compile = flip evalState 0 . compileM

-- Here I chose a fully recursive implementation (each recursive call
-- produces a full NFA; auxiliary states glue them together), as opposed to
-- a continuation-based semi-recursive implementation (a recursive call
-- accepts exit states, returns start states and transitions) used in
-- regex-applicative and in
-- https://ro-che.info/articles/2015-09-02-monadfix. No particular reason.
compileM
  :: Ord c
  => Regex c
  -> State Int (NFA (Maybe c))
compileM = \case
  Sym c -> do
    s <- fresh
    f <- fresh
    return $ NFA
      { nfaStart = Set.singleton s
      , nfaFinal = Set.singleton f
      , nfaTransitions = transitionsFromList [(Just c, s, f)]
      }
  Empty -> do
    s <- fresh
    return $ NFA
      { nfaStart = Set.singleton s
      , nfaFinal = Set.singleton s
      , nfaTransitions = transitionsFromList []
      }
  Seq a b -> do
    nfa_a <- compileM a
    nfa_b <- compileM b
    return $ NFA
      { nfaStart = nfaStart nfa_a
      , nfaFinal = nfaFinal nfa_b
      , nfaTransitions = mconcat
        [ nfaTransitions nfa_a
        , nfaTransitions nfa_b
        , transitionsFromList
          [ (Nothing, final_a, start_b)
          | final_a <- Set.toList (nfaFinal nfa_a)
          , start_b <- Set.toList (nfaStart nfa_b)
          ]
        ]
      }
  Alt a b -> do
    nfa_a <- compileM a
    nfa_b <- compileM b
    return $ NFA
      { nfaStart = nfaStart nfa_a <> nfaStart nfa_b
      , nfaFinal = nfaFinal nfa_a <> nfaFinal nfa_b
      , nfaTransitions = nfaTransitions nfa_a <> nfaTransitions nfa_b
      }
  Plus a -> do
    nfa_a <- compileM a
    return $ NFA
      { nfaStart = nfaStart nfa_a
      , nfaFinal = nfaFinal nfa_a
      , nfaTransitions =
        nfaTransitions nfa_a <>
        transitionsFromList
        [ (Nothing, f, s)
        | s <- Set.toList $ nfaStart nfa_a
        , f <- Set.toList $ nfaFinal nfa_a
        ]
      }
  Star a -> compileM $ Alt (Plus a) Empty

  where
    fresh :: State Int NfaState
    fresh = do
      n <- get
      put $! n+1
      return $ NfaState n

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
