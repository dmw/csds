----------------------------------------------------------------------------
-- |
-- Module      :  Data.CSDS.DequeLSpec
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  http://coder.cl/products/csds/
-- Repository  :  https://github.com/dmw/csds
--
-- A Core Stateful Data Structures, Deque based on List Testing module
--
-- This is the initial commit of this project, please write
-- me directly if you want to contribute.
-----------------------------------------------------------------------------

module Data.CSDS.DequeLSpec (spec) where


import Data.Monoid
import Data.CSDS.DequeL
import Test.Hspec


testPushAndPop :: Bool
testPushAndPop = let x :: DequeLT Int
                     x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                     (y, z) = popQ x
                 in toListQ x == [5, 10, 20]
                        && toListQ z == [10, 20, 5]
                        && y == 5


testReversePop :: [Int]
testReversePop = let x :: DequeLT Int
                     x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                     y = reverseQ x
                 in toListQ y


testSortPop :: [Int]
testSortPop = let x :: DequeLT Int
                  x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                  z = sortQ x
              in toListQ z


testMonoidMappend :: [Int]
testMonoidMappend = let x :: DequeLT Int
                        y :: DequeLT Int
                        x = pushQ 3 $ pushQ 2 $ pushQ 1 emptyQ
                        y = pushQ 6 $ pushQ 5 $ pushQ 4 emptyQ
                        z = x `mappend` y
                    in toListQ z


testMonoidMconcat :: [Int]
testMonoidMconcat = let x :: DequeLT Int
                        y :: DequeLT Int
                        x = pushQ 3 $ pushQ 2 $ pushQ 1 emptyQ
                        y = pushQ 6 $ pushQ 5 $ pushQ 4 emptyQ
                        r = mconcat [x, y]
                    in toListQ r


testFoldLQ :: [Int]
testFoldLQ = let x :: DequeLT Int
                 x = pushQ 25 $ pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                 r = foldlQ (flip pushQ) x
             in toListQ r


testMapConcat :: [Int]
testMapConcat = let x :: DequeLT Int
                    r :: [DequeLT Int]
                    x = pushQ 1 $ pushQ 2 $ pushQ 3 $ pushQ 4 emptyQ
                    r = map singletonQ $ toListQ x
                    m = mapConcatQ (+ 5) r
                in toListQ m


spec :: Spec
spec = do it "check DequeL pushQ and popQ operations" $
             testPushAndPop `shouldBe` True

          it "check DequeL reverseQ operations" $
             testReversePop `shouldBe` [20, 10, 5]

          it "check DequeL sortQ operations" $
             testSortPop `shouldBe` [5, 10, 20]

          it "check DequeL Monoid mappend operations" $
             testMonoidMappend `shouldBe` [1, 2, 3, 4, 5, 6]

          it "check DequeL Monoid mconcat operations" $
             testMonoidMconcat `shouldBe` [1, 2, 3, 4, 5, 6]

          it "check DequeL foldLQ operations" $
             testFoldLQ `shouldBe` [5,10]

          it "check DequeL mapConcatQ operations" $
             testMapConcat `shouldBe` [9,8,7,6]
