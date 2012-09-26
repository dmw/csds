----------------------------------------------------------------------------
-- |
-- Module      :  Data.CSDS.QueueLSpec
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  http://coder.cl/products/csds/
-- Repository  :  https://github.com/dmw/csds
--
-- A Core Stateful Data Structures, Queue based on List Testing module
--
-- This is the initial commit of this project, please write
-- me directly if you want to contribute.
-----------------------------------------------------------------------------

module Data.CSDS.QueueLSpec (spec) where


import Data.Monoid
import Data.CSDS.QueueL
import Test.Hspec


testPushAndPop :: Bool
testPushAndPop = let x :: QueueLT Int
                     x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                     (y, z) = popQ x
                 in toListQ x == [5, 10, 20]
                        && toListQ z == [10, 20]
                               && y == 5


testReverseAndSortPop :: Bool
testReverseAndSortPop = let x :: QueueLT Int
                            x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                            y = reverseQ x
                            z = sortQ x
                 in toListQ y == [20, 10, 5]
                        && toListQ z == [5, 10, 20]


testMonoidMappend :: Bool
testMonoidMappend = let x :: QueueLT Int
                        y :: QueueLT Int
                        x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                        y = pushQ 5 $ pushQ 15 $ pushQ 25 emptyQ
                        z = x `mappend` y
                    in toListQ z == [25, 15, 5, 5, 10, 20]


testMonoidMconcat :: Bool
testMonoidMconcat = let x :: QueueLT Int
                        y :: QueueLT Int
                        x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                        y = pushQ 5 $ pushQ 15 $ pushQ 25 emptyQ
                        r = mconcat [x, y]
                    in toListQ r == [25, 15, 5, 5, 10, 20]


testFoldLQ :: Bool
testFoldLQ = let x :: QueueLT Int
                 x = pushQ 25 $ pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                 r = foldlQ (\ n y -> pushQ y n) x
             in toListQ r == [5, 10, 20, 25]


testFMap :: Bool
testFMap = let x :: QueueLT Int
               x = pushQ 25 $ pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
               r = fmap (+ 5) x
           in toListQ r == [10, 15, 25, 30]


testMapConcat :: Bool
testMapConcat = let x :: QueueLT Int
                    r :: [QueueLT Int]
                    x = pushQ 25 $ pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                    r = map singletonQ $ toListQ x
                    m = mapConcatQ (\ y -> y + 5) r
                    n = fmap (+ 5) x
                in toListQ n == toListQ m


spec :: Spec
spec = do it "check QueueL pushQ and popQ operations" $
             testPushAndPop `shouldBe` True
          it "check QueueL sortQ and reverseQ operations" $
             testReverseAndSortPop `shouldBe` True
          it "check QueueL Monoid mappend operations" $
             testMonoidMappend `shouldBe` True
          it "check QueueL Monoid mconcat operations" $
             testMonoidMconcat `shouldBe` True
          it "check QueueL foldLQ operations" $
             testFoldLQ `shouldBe` True
          it "check QueueL fmap operations" $
             testFMap `shouldBe` True
          it "check QueueL mapConcatQ operations" $
             testMapConcat `shouldBe` True
