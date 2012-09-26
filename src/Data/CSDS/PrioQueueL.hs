{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.CSDS.PrioQueueL
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  http://coder.cl/products/csds/
-- Repository  :  https://github.com/dmw/csds
--
-- A Core Stateful Data Structures, Queue based on List module
--
-- This is the initial commit of this project, please write
-- me directly if you want to contribute.
-----------------------------------------------------------------------------

module Data.CSDS.PrioQueueL
    (
     PrioQueueLClass (..)
    , PrioQueueLT (..)
    ) where


import qualified Data.List        as L
import Data.Monoid


-- | Queue Type definition, it should be instantiated as PrioQueueL Type.
--
-- Please review the example below.
-- > PrioQueueLT Int
newtype PrioQueueLT t = PrioQueueLT (Int, [t]) deriving (Ord, Eq, Show)


-- | Queue (List Based) Class Definition
--
-- Core functions for any Queue (based on List)
--
class (Ord t, Eq t) => PrioQueueLClass t where

    -- | Binging Type
    type SLT t

    -- | Returns an empty Queue
    emptyQ :: t

    -- | Checks if the queue is empty (True if the queue is empty)
    isEmptyQ :: t -> Bool

    -- | Push an item into the queue
    pushQ :: SLT t -> t -> t

    -- | Pops an item into the queue
    popQ :: t -> (SLT t, t)

    -- | Returns the queue size or length
    lengthQ :: t -> Int

    -- | Reverse the queue
    reverseQ :: t -> t

    -- | Creates a queue from a list
    fromListQ :: [SLT t] -> t

    -- | Converts a queue to list
    toListQ :: t -> [SLT t]

    -- | Returns the queue size
    queueLTSize :: t -> Int

    -- | Returns the queue internal list
    queueLTList :: t -> [SLT t]

    -- | Sorts a list if the binding type implements Ord
    sortQ :: t -> t

    -- | Folds a queue returning an empty queue because uses popS for folding
    foldlQ :: (t -> SLT t -> t) -> t -> t

    -- | Implements mapContact for two lists of binding types returning a queue
    mapConcatQ :: (SLT t -> SLT t) -> [t] -> t

    -- | Creates a singleton queue
    singletonQ :: SLT t -> t


-- | PrioQueueLT instance for PrioQueueLT
--
-- Core functions for any PrioQueueLT (based on List)
--
instance (Ord t, Eq t) => PrioQueueLClass (PrioQueueLT t) where

    -- | Binging Type as (PrioQueueLT)
    type SLT (PrioQueueLT t) = t

    -- | Returns an empty PrioQueueLT
    emptyQ = PrioQueueLT (0, [])

    -- | Checks if the PrioQueueLT is empty (True if the queue is empty)
    isEmptyQ x = lengthQ x == 0

    -- | Push an item into the PrioQueueLT
    pushQ s rs = let x = queueLTSize rs
                     y = queueLTList rs
                 in sortQ $ PrioQueueLT (x + 1, y ++ [s])


    -- | Pops an item into the PrioQueueLT
    popQ rs = let x = queueLTSize rs
                  y = queueLTList rs
              in (head y, (sortQ $ PrioQueueLT (x - 1, tail y)))

    -- | Returns the PrioQueueLT size or length
    lengthQ = queueLTSize

    -- | Reverse the PrioQueueLT
    reverseQ rs = let x = queueLTSize rs
                      y = queueLTList rs
                  in PrioQueueLT (x, reverse y)

    -- | Creates a PrioQueueLT from a list
    fromListQ x = PrioQueueLT (length x, x)

    -- | Returns the PrioQueueLT as List
    toListQ = queueLTList

    -- | Returns the PrioQueueLT size
    queueLTSize (PrioQueueLT (x, _)) = x

    -- | Returns the PrioQueueLT internal List
    queueLTList (PrioQueueLT (_, xs)) = xs

    -- | Sorts a list if the binding type implements Ord
    sortQ rs = let x = queueLTSize rs
                   y = L.sort $ queueLTList rs
               in PrioQueueLT (x, y)

    -- | Folds a queue returning an empty queue because uses popS for folding
    foldlQ f xs = let e = emptyQ
                      foldlS' ms r
                          | isEmptyQ ms = r
                          | otherwise = let (ny, ns) = popQ ms
                                        in foldlS' ns (f r ny)
                  in foldlS' xs e


    -- | Implements mapContact for two lists of binding types returning a queue
    mapConcatQ f xs = let fl = concatMap (map f . toListQ) xs
                          ln = length fl
                      in sortQ $ PrioQueueLT (ln, fl)


    -- | Creates a singleton queue
    singletonQ i = PrioQueueLT (1, [i])


-- | PrioQueueLT instance for Monoid
--
-- Core Monoid for any PrioQueueLT (based on List)
-- A class for monoids (types with an associative binary operation
-- that has an identity) with various general-purpose instances.
--
instance (Ord t, Eq t) => Monoid (PrioQueueLT t) where

    -- | Identity of mappend
    mempty = PrioQueueLT (0, [])

    -- | An associative operation (concatenation)
    mappend a b = let x = queueLTSize a
                      y = queueLTList a
                      m = queueLTSize b
                      n = queueLTList b
                  in sortQ $ PrioQueueLT (x + m, y `mappend` n)

    -- | Fold a list using the monoid.
    -- For most types, the default definition for mconcat will be used,
    -- but the function is included in the class definition so that an
    -- optimized version can be provided for specific types.
    mconcat a = let foldLT (x:xs) r = foldLT xs $ r `mappend` x
                    foldLT [] r = sortQ $ r
                in foldLT a mempty

