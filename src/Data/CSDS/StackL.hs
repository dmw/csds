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
    , singletonSF
    ) where


import qualified Data.Foldable    as F
import qualified Data.List        as L
import Data.Monoid

-- | Stack Type definition, it should be instantiated as StackL Type.
--
-- Please review the example below.
-- > StackLT Int
newtype StackLT t = StackLT (Int, [t]) deriving (Eq, Ord, Show)


singletonSF :: t -> StackLT t
singletonSF x = StackLT (1, [x])


-- | Stack (List Based) Class Definition
--
-- Core functions for any Stack (based on List)
--
class StackLClass t where

    -- | Binging Type
    type SLT t

    -- | Returns an empty Stack
    emptyS :: t

    -- | Checks if the stack is empty (True if the stack is empty)
    isEmptyS :: t -> Bool

    -- | Push an item into the stack
    pushS :: SLT t -> t -> t

    -- | Pops an item into the stack
    popS :: t -> (SLT t, t)

    -- | Returns the stack size or length
    lengthS :: t -> Int

    -- | Reverse the stack
    reverseS :: t -> t

    -- | Creates a stack from a list
    fromListS :: [SLT t] -> t

    -- | Converts a stack to list
    toListS :: t -> [SLT t]

    -- | Returns the stack size
    stackLTSize :: t -> Int

    -- | Returns the stack internal list
    stackLTList :: t -> [SLT t]

    -- | Sorts a list if the binding type implements Ord
    sortS :: (Ord (SLT t)) => t -> t

    -- | Folds a stack returning an empty stack because uses popS for folding
    foldlS :: (t -> SLT t -> t) -> t -> t

    -- | Implements mapContact for two lists of binding types returning a stack
    mapConcatS :: (SLT t -> SLT t) -> [t] -> t

    -- | Creates a singleton stack
    singletonS :: SLT t -> t

    -- | Creates a list of singletons
    singletonLS :: [SLT t] -> [t]


-- | StackLT instance for StackLT
--
-- Core functions for any StackLT (based on List)
--
instance StackLClass (StackLT t) where

    -- | Binging Type as (StackLT)
    type SLT (StackLT t) = t

    -- | Returns an empty StackLT
    emptyS = StackLT (0, [])

    -- | Checks if the StackLT is empty (True if the stack is empty)
    isEmptyS x = lengthS x == 0

    -- | Push an item into the StackLT
    pushS s rs = let x = stackLTSize rs
                     y = stackLTList rs
                 in StackLT (x + 1, s : y)


    -- | Pops an item into the StackLT
    popS rs = let x = stackLTSize rs
                  y = stackLTList rs
              in (head y, StackLT (x - 1, tail y))

    -- | Returns the StackLT size or length
    lengthS = stackLTSize

    -- | Reverse the StackLT
    reverseS rs = let x = stackLTSize rs
                      y = stackLTList rs
                  in StackLT (x, reverse y)

    -- | Creates a StackLT from a list
    fromListS x = StackLT (length x, x)

    -- | Returns the StackLT as List
    toListS = stackLTList

    -- | Returns the StackLT size
    stackLTSize (StackLT (x, _)) = x

    -- | Returns the StackLT internal List
    stackLTList (StackLT (_, xs)) = xs

    -- | Sorts a list if the binding type implements Ord
    sortS rs = let x = stackLTSize rs
                   y = stackLTList rs
               in StackLT (x, L.sort y)

    -- | Folds a stack returning an empty stack because uses popS for folding
    foldlS f xs = let e = emptyS
                      foldlS' ms r
                          | isEmptyS ms = r
                          | otherwise = let (ny, ns) = popS ms
                                        in foldlS' ns (f r ny)
                  in foldlS' xs e


    -- | Implements mapContact for two lists of binding types returning a stack
    mapConcatS f xs = let fl = concatMap (map f . toListS) xs
                          ln = length fl
                      in StackLT (ln, fl)


    -- | Creates a singleton stack
    singletonS i = StackLT (1, [i])

    -- | Creates a list of singletons
    singletonLS xs = let singletonLS' :: [t] -> [StackLT t] -> [StackLT t]
                         singletonLS' rs n = foldl (\ y r -> y ++ [singletonS r]) n rs
                     in singletonLS' xs []


-- | StackLT instance for Functor
--
-- Core Functor for any StackLT (based on List)
-- Uniform action over a parameterized type, generalizing the map
-- function on lists.
--
instance Functor (StackLT) where

    -- | Uniform action over a parameterized type.
    fmap f rs = let x = stackLTSize rs
                    y = stackLTList rs
                in StackLT (x, fmap f y)


-- | StackLT instance for Monoid
--
-- Core Monoid for any StackLT (based on List)
-- A class for monoids (types with an associative binary operation
-- that has an identity) with various general-purpose instances.
--
instance Monoid (StackLT t) where

    -- | Identity of mappend
    mempty = StackLT (0, [])

    -- | An associative operation (concatenation)
    mappend a b = let x = stackLTSize a
                      y = stackLTList a
                      m = stackLTSize b
                      n = stackLTList b
                  in StackLT (x + m, y `mappend` n)

    -- | Fold a list using the monoid.
    -- For most types, the default definition for mconcat will be used,
    -- but the function is included in the class definition so that an
    -- optimized version can be provided for specific types.
    mconcat a = let foldLT xs r = foldl mappend r xs
                in foldLT a mempty


-- | StackLT instance for Foldable
--
-- Core Foldable for any StackLT (based on List)
-- Class of data structures that can be folded to a summary value.
-- Many of these functions generalize Prelude, Control.Monad and Data.List
-- functions of the same names from lists to any Foldable functor. To avoid
-- ambiguity, either import those modules hiding these names or qualify uses
-- of these function names with an alias for this module.
--
instance F.Foldable (StackLT) where

    -- | Combine the elements of a structure using a monoid.
    fold m = mconcat $ toListS m

    -- | Map each element of the structure to a monoid, and combine the results.
    foldMap f m = mconcat (fmap f $ toListS m)

    -- | Right-associative fold of a structure.
    foldr f n m = L.foldr f n (toListS m)

    -- | Left-associative fold of a structure.
    foldl f n m = L.foldl f n (toListS m)

    -- | A variant of foldr that has no base case, and thus may only be applied
    -- to non-empty structures.
    foldr1 f m = L.foldr1 f (toListS m)

    -- | A variant of foldl that has no base case, and thus may only
    -- be applied to non-empty structures.
    foldl1 f m = L.foldl1 f (toListS m)


-- | The Monad instance for StackLT based on its Monoid
instance Monad (StackLT) where
    -- | Inject a value into the monadic type.
    return x = StackLT (1, [x])

    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (StackLT (_, xs)) >>= g = mconcat (map g xs)
