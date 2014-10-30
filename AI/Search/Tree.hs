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
module AI.Search.Tree where

type Adjs     a = a -> [a]
type Goal     a = a -> Bool
type Strategy a = [a]  -- ^ states
               -> [a]  -- ^ children
               -> [a]  -- ^ updated states

breadthFirstSearch :: a -> Adjs a -> Goal a -> Maybe a
breadthFirstSearch state adjs goal
  | null results = Nothing
  | otherwise    = Just (head results)
  where
    results = filter goal $ breadthFirstTreeList state adjs

breadthFirstTreeLevels :: a -> Adjs a -> [[a]]
breadthFirstTreeLevels state adjs =
  takeWhile (not . null) (iterate (concatMap adjs) [state])

breadthFirstTreeList :: a -> Adjs a -> [a]
breadthFirstTreeList state adjs =
  concat $ breadthFirstTreeLevels state adjs

treeSearch :: [a] -> Strategy a -> Adjs a -> Goal a -> Maybe a
treeSearch [] _ _ _ = Nothing
treeSearch (x:xs) strategy adjs goal
  | goal x    = Just x
  | otherwise = treeSearch (strategy xs (adjs x)) strategy adjs goal

breadthFirstStrategy :: Strategy a
breadthFirstStrategy = (++)

depthFirstStrategy :: Strategy a
depthFirstStrategy  = flip (++)
