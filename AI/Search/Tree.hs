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

treeSearch :: [a] -> Strategy a -> Adjs a -> Goal a -> Maybe a
treeSearch [] _ _ _ = Nothing
treeSearch (x:xs) strategy adjs goal
  | goal x    = Just x
  | otherwise = treeSearch (strategy xs (adjs x)) strategy adjs goal

breadthFirstStrategy :: Strategy a
breadthFirstStrategy = (++)

depthFirstStrategy :: Strategy a
depthFirstStrategy  = flip (++)

main :: IO ()
main = do
  print "aaaa"
  print "bbb"
  print "ccc"

main = print "aaa" >>= (\_ -> print "bbb") >>= (\_ -> print "ccc")
