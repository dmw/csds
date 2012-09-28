{-# LANGUAGE TypeFamilies, FlexibleContexts, GADTs #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Data.CSDS.DequeL
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  http://coder.cl/products/csds/
-- Repository  :  https://github.com/dmw/csds
--
-- A Core Stateful Data Structures, Deque based on List module
--
-- This is the initial commit of this project, please write
-- me directly if you want to contribute.
-----------------------------------------------------------------------------

module Data.CSDS.DequeL
    (
     DequeLClass (..)
    , DequeLT (..)
    ) where


import qualified Data.List        as L
import Data.Monoid

-- | Deque Type definition, it should be instantiated as DequeL Type.
--
-- Please review the example below.
-- > DequeLT Int
newtype DequeLT t = DequeLT (Int, [t]) deriving (Eq, Ord, Show)


-- | Deque (List Based) Class Definition
--
-- Core functions for any Deque (based on List)
--
class (Eq t) => DequeLClass t where

    -- | Binging Type
    type SLT t

    -- | Returns an empty Deque
    emptyQ :: t

    -- | Checks if the deque is empty (True if the deque is empty)
    isEmptyQ :: t -> Bool

    -- | Push an item into the deque
    pushQ :: SLT t -> t -> t

    -- | Pops an item into the deque
    popQ :: t -> (SLT t, t)

    -- | Returns the deque size or length
    lengthQ :: t -> Int

    -- | Reverse the deque
    reverseQ :: t -> t

    -- | Creates a deque from a list
    fromListQ :: [SLT t] -> t

    -- | Converts a deque to list
    toListQ :: t -> [SLT t]

    -- | Returns the deque size
    dequeLTSize :: t -> Int

    -- | Returns the deque internal list
    dequeLTList :: t -> [SLT t]

    -- | Sorts a list if the binding type implements Ord
    sortQ :: (Ord (SLT t)) => t -> t

    -- | Folds a deque returning an empty deque because uses popS for folding
    foldlQ :: (t -> SLT t -> t) -> t -> t

    -- | Implements mapContact for two lists of binding types returning a deque
    mapConcatQ :: (SLT t -> SLT t) -> [t] -> t

    -- | Creates a singleton deque
    singletonQ :: SLT t -> t


-- | DequeLT instance for DequeLT
--
-- Core functions for any DequeLT (based on List)
--
instance (Eq t) => DequeLClass (DequeLT t) where

    -- | Binging Type as (DequeLT)
    type SLT (DequeLT t) = t

    -- | Returns an empty DequeLT
    emptyQ = DequeLT (0, [])

    -- | Checks if the DequeLT is empty (True if the deque is empty)
    isEmptyQ x = lengthQ x == 0

    -- | Push an item into the DequeLT
    pushQ s rs = let x = dequeLTSize rs
                     y = dequeLTList rs
                 in DequeLT (x + 1, y ++ [s])


    -- | Pops an item into the DequeLT
    popQ rs = let x = dequeLTSize rs
                  y = dequeLTList rs
                  n = head y
                  m = tail y
                  p = m ++ [n]
              in (n, DequeLT (x, p))

    -- | Returns the DequeLT size or length
    lengthQ = dequeLTSize

    -- | Reverse the DequeLT
    reverseQ rs = let x = dequeLTSize rs
                      y = dequeLTList rs
                  in DequeLT (x, reverse y)

    -- | Creates a DequeLT from a list
    fromListQ x = DequeLT (length x, x)

    -- | Returns the DequeLT as List
    toListQ = dequeLTList

    -- | Returns the DequeLT size
    dequeLTSize (DequeLT (x, _)) = x

    -- | Returns the DequeLT internal List
    dequeLTList (DequeLT (_, xs)) = xs

    -- | Sorts a list if the binding type implements Ord
    sortQ rs = let x = dequeLTSize rs
                   y = dequeLTList rs
               in DequeLT (x, L.sort y)

    -- | Folds a deque returning an empty deque because uses popS for folding
    foldlQ f xs = let e = emptyQ
                      h = head $ toListQ xs
                      foldlS' ms r d
                          | isEmptyQ ms = r
                          | d == True = r
                          | otherwise = let (ny, ns) = popQ ms
                                        in if h == ny
                                           then foldlS' ns (f r ny) False
                                           else foldlS' ns (f r ny) True
                  in foldlS' xs e False


    -- | Implements mapContact for two lists of binding types returning a deque
    mapConcatQ f xs = let fl = concatMap (map f . toListQ) xs
                          ln = length fl
                      in DequeLT (ln, fl)


    -- | Creates a singleton deque
    singletonQ i = DequeLT (1, [i])


-- | DequeLT instance for Monoid
--
-- Core Monoid for any DequeLT (based on List)
-- A class for monoids (types with an associative binary operation
-- that has an identity) with various general-purpose instances.
--
instance (Eq t) => Monoid (DequeLT t) where

    -- | Identity of mappend
    mempty = DequeLT (0, [])

    -- | An associative operation (concatenation)
    mappend a b = let x = dequeLTSize a
                      y = dequeLTList a
                      m = dequeLTSize b
                      n = dequeLTList b
                  in DequeLT (x + m, y `mappend` n)

    -- | Fold a list using the monoid.
    -- For most types, the default definition for mconcat will be used,
    -- but the function is included in the class definition so that an
    -- optimized version can be provided for specific types.
    mconcat a = let foldLT xs r = foldl mappend r xs
                in foldLT a mempty
