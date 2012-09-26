{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.CSDS.StackL
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  http://coder.cl/products/csds/
-- Repository  :  https://github.com/dmw/csds
--
-- A Core Stateful Data Structures, Stack based on List module
--
-- This is the initial commit of this project, please write
-- me directly if you want to contribute.
-----------------------------------------------------------------------------

module Data.CSDS.StackL
    (
     StackLClass (..)
    , StackLT (..)
    ) where


import qualified Data.Foldable    as F
import qualified Data.List        as L
import Data.Monoid


newtype StackLT t = StackLT (Int, [t]) deriving (Eq, Ord, Show)


class StackLClass t where

    type SLT t

    emptyS :: t

    isEmptyS :: t -> Bool

    pushS :: SLT t -> t -> t

    popS :: t -> (SLT t, t)

    lengthS :: t -> Int

    reverseS :: t -> t

    fromListS :: [SLT t] -> t

    toListS :: t -> [SLT t]

    stackLTSize :: t -> Int

    stackLTList :: t -> [SLT t]

    sortS :: (Ord (SLT t)) => t -> t

    foldlS :: (t -> SLT t -> t) -> t -> t

    mapConcatS :: (SLT t -> SLT t) -> [t] -> t

    singletonS :: SLT t -> t

    singletonLS :: [SLT t] -> [t]


instance StackLClass (StackLT t) where

    type SLT (StackLT t) = t

    emptyS = StackLT (0, [])

    isEmptyS x = lengthS x == 0

    pushS s rs = let x = stackLTSize rs
                     y = stackLTList rs
                 in StackLT (x + 1, (s:y))


    popS rs = let x = stackLTSize rs
                  y = stackLTList rs
              in (head y, StackLT (x - 1, tail y))

    lengthS rs = stackLTSize rs

    reverseS rs = let x = stackLTSize rs
                      y = stackLTList rs
                  in StackLT (x, reverse y)

    fromListS x = StackLT (length x, x)

    toListS = stackLTList

    stackLTSize (StackLT (x, _)) = x

    stackLTList (StackLT (_, xs)) = xs


    sortS rs = let x = stackLTSize rs
                   y = stackLTList rs
               in StackLT (x, L.sort y)

    foldlS f xs = let e = emptyS
                      foldlS' ms r
                          | isEmptyS ms = r
                          | otherwise = let (ny, ns) = popS ms
                                        in foldlS' ns (f r ny)
                  in foldlS' xs e


    mapConcatS f xs = let fl = concatMap (map f . toListS) xs
                          ln = length fl
                      in StackLT (ln, fl)


    singletonS i = StackLT (1, [i])

    singletonLS xs = let singletonLS' :: [t] -> [StackLT t] -> [StackLT t]
                         singletonLS' (r:rs) n = singletonLS' rs (n ++ [singletonS r])
                         singletonLS' [] n = n
                     in singletonLS' xs []


instance Functor (StackLT) where

    fmap f rs = let x = stackLTSize rs
                    y = stackLTList rs
                in StackLT (x, fmap f y)


instance Monoid (StackLT t) where

    mempty = StackLT (0, [])

    mappend a b = let x = stackLTSize a
                      y = stackLTList a
                      m = stackLTSize b
                      n = stackLTList b
                  in StackLT (x + m, y `mappend` n)

    mconcat a = let foldLT (x:xs) r = foldLT xs $ r `mappend` x
                    foldLT [] r = r
                in foldLT a mempty



instance F.Foldable (StackLT) where

    fold m = mconcat $ toListS m

    foldMap f m = mconcat (fmap f $ toListS m)

    foldr f n m = L.foldr f n (toListS m)

    foldl f n m = L.foldl f n (toListS m)

    foldr1 f m = L.foldr1 f (toListS m)

    foldl1 f m = L.foldl1 f (toListS m)


