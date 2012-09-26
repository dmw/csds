{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.CSDS.QueueL
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

module Data.CSDS.QueueL
    (
     QueueLClass (..)
    , QueueLT (..)
    ) where


import qualified Data.Foldable    as F
import qualified Data.List        as L
import Data.Monoid

-- | Queue Type definition, it should be instantiated as QueueL Type.
--
-- Please review the example below.
-- > QueueLT Int
newtype QueueLT t = QueueLT (Int, [t]) deriving (Eq, Ord, Show)


-- | Queue (List Based) Class Definition
--
-- Core functions for any Queue (based on List)
--
class QueueLClass t where

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
    sortQ :: (Ord (SLT t)) => t -> t

    -- | Folds a queue returning an empty queue because uses popS for folding
    foldlQ :: (t -> SLT t -> t) -> t -> t

    -- | Implements mapContact for two lists of binding types returning a queue
    mapConcatQ :: (SLT t -> SLT t) -> [t] -> t

    -- | Creates a singleton queue
    singletonQ :: SLT t -> t

    -- | Creates a list of singletons
    singletonLQ :: [SLT t] -> [t]


-- | QueueLT instance for QueueLT
--
-- Core functions for any QueueLT (based on List)
--
instance QueueLClass (QueueLT t) where

    -- | Binging Type as (QueueLT)
    type SLT (QueueLT t) = t

    -- | Returns an empty QueueLT
    emptyQ = QueueLT (0, [])

    -- | Checks if the QueueLT is empty (True if the queue is empty)
    isEmptyQ x = lengthQ x == 0

    -- | Push an item into the QueueLT
    pushQ s rs = let x = queueLTSize rs
                     y = queueLTList rs
                 in QueueLT (x + 1, y ++ [s])


    -- | Pops an item into the QueueLT
    popQ rs = let x = queueLTSize rs
                  y = queueLTList rs
              in (head y, QueueLT (x - 1, tail y))

    -- | Returns the QueueLT size or length
    lengthQ = queueLTSize

    -- | Reverse the QueueLT
    reverseQ rs = let x = queueLTSize rs
                      y = queueLTList rs
                  in QueueLT (x, reverse y)

    -- | Creates a QueueLT from a list
    fromListQ x = QueueLT (length x, x)

    -- | Returns the QueueLT as List
    toListQ = queueLTList

    -- | Returns the QueueLT size
    queueLTSize (QueueLT (x, _)) = x

    -- | Returns the QueueLT internal List
    queueLTList (QueueLT (_, xs)) = xs

    -- | Sorts a list if the binding type implements Ord
    sortQ rs = let x = queueLTSize rs
                   y = queueLTList rs
               in QueueLT (x, L.sort y)

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
                      in QueueLT (ln, fl)


    -- | Creates a singleton queue
    singletonQ i = QueueLT (1, [i])

    -- | Creates a list of singletons
    singletonLQ xs = let singletonLQ' :: [t] -> [QueueLT t] -> [QueueLT t]
                         singletonLQ' rs n = foldl (\ y r -> y ++ [singletonQ r]) n rs
                     in singletonLQ' xs []


-- | QueueLT instance for Functor
--
-- Core Functor for any QueueLT (based on List)
-- Uniform action over a parameterized type, generalizing the map
-- function on lists.
--
instance Functor (QueueLT) where

    -- | Uniform action over a parameterized type.
    fmap f rs = let x = queueLTSize rs
                    y = queueLTList rs
                in QueueLT (x, fmap f y)


-- | QueueLT instance for Monoid
--
-- Core Monoid for any QueueLT (based on List)
-- A class for monoids (types with an associative binary operation
-- that has an identity) with various general-purpose instances.
--
instance Monoid (QueueLT t) where

    -- | Identity of mappend
    mempty = QueueLT (0, [])

    -- | An associative operation (concatenation)
    mappend a b = let x = queueLTSize a
                      y = queueLTList a
                      m = queueLTSize b
                      n = queueLTList b
                  in QueueLT (x + m, n `mappend` y)

    -- | Fold a list using the monoid.
    -- For most types, the default definition for mconcat will be used,
    -- but the function is included in the class definition so that an
    -- optimized version can be provided for specific types.
    mconcat a = let foldLT (x:xs) r = foldLT xs $ r `mappend` x
                    foldLT [] r = r
                in foldLT a mempty


-- | QueueLT instance for Foldable
--
-- Core Foldable for any QueueLT (based on List)
-- Class of data structures that can be folded to a summary value.
-- Many of these functions generalize Prelude, Control.Monad and Data.List
-- functions of the same names from lists to any Foldable functor. To avoid
-- ambiguity, either import those modules hiding these names or qualify uses
-- of these function names with an alias for this module.
--
instance F.Foldable (QueueLT) where

    -- | Combine the elements of a structure using a monoid.
    fold m = mconcat $ toListQ m

    -- | Map each element of the structure to a monoid, and combine the results.
    foldMap f m = mconcat (fmap f $ toListQ m)

    -- | Right-associative fold of a structure.
    foldr f n m = L.foldr f n (toListQ m)

    -- | Left-associative fold of a structure.
    foldl f n m = L.foldl f n (toListQ m)

    -- | A variant of foldr that has no base case, and thus may only be applied
    -- to non-empty structures.
    foldr1 f m = L.foldr1 f (toListQ m)

    -- | A variant of foldl that has no base case, and thus may only
    -- be applied to non-empty structures.
    foldl1 f m = L.foldl1 f (toListQ m)
