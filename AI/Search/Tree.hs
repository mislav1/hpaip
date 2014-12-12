{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
{-# OPTIONS_GHC -W -Wall #-}
------------------------------------------------------------------------
-- |
-- Module      : AI.Search.Tree
-- Copyright   : (c) 2014 Konrad Grzanek
-- License     : BSD-style (see the file LICENSE)
-- Created     : 2014-10-25
-- Maintainer  : kongra@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- (Rose) tree search algorithms
------------------------------------------------------------------------
module AI.Search.Tree
    (
      -- * Search abstraction
      Adjs
    , Goal
    , Strategy

      -- * Tree traversal mechanisms
    , breadthFirstSearch
    , breadthFirstTreeLevels

      -- * Tree search
    , treeSearch
    , breadthFirstStrategy
    , depthFirstStrategy
    )
    where

-- | The type of functions that generate children of a given (rose)
-- tree node.
type Adjs a = a -> [a]

-- | Goal of the tree search process.
type Goal a = a -> Bool

-- | A tree search strategy.
type Strategy a = [a]  -- ^ states
               -> [a]  -- ^ children
               -> [a]  -- ^ updated states

-- | Searches a tree using breadth-first strategy.
breadthFirstSearch :: a -> Adjs a -> Goal a -> Maybe a
breadthFirstSearch state adjs goal
  | null results = Nothing
  | otherwise    = Just (head results)
  where
    results = filter goal $ breadthFirstTreeList state adjs
{-# INLINE breadthFirstSearch #-}

-- | Returns a list of levels in a tree traversed using breadth-first
-- strategy.
breadthFirstTreeLevels :: a -> Adjs a -> [[a]]
breadthFirstTreeLevels state adjs =
  takeWhile (not . null) (iterate (concatMap adjs) [state])

breadthFirstTreeList :: a -> Adjs a -> [a]
breadthFirstTreeList state adjs =
  concat (breadthFirstTreeLevels state adjs)

-- | A generic, strategy-agnostic tree search algorithm.
treeSearch :: [a] -> Strategy a -> Adjs a -> Goal a -> Maybe a
treeSearch [] _ _ _ = Nothing
treeSearch (x:xs) strategy adjs goal
  | goal x    = Just x
  | otherwise = treeSearch (strategy xs (adjs x)) strategy adjs goal

breadthFirstStrategy :: Strategy a
breadthFirstStrategy = (++)

depthFirstStrategy :: Strategy a
depthFirstStrategy  = flip (++)

-- EX 1

-- tree1Adjs :: Adjs String
-- tree1Adjs "a" = ["b", "c"]
-- tree1Adjs "b" = ["d", "e"]
-- tree1Adjs "c" = ["f", "g"]
-- tree1Adjs "e" = ["h", "i"]
-- tree1Adjs "f" = ["j", "k", "l"]
-- tree1Adjs "g" = ["m", "n"]
-- tree1Adjs _   = []

-- goal1 :: String -> Goal String
-- goal1 s1 s2 = s1 == s2

-- excercise1 :: Maybe String
-- excercise1 = treeSearch ["a"] depthFirstStrategy tree1Adjs (goal1 "goat")
