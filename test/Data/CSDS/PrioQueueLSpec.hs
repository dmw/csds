----------------------------------------------------------------------------
-- |
-- Module      :  Data.CSDS.PrioQueueLSpec
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  http://coder.cl/products/csds/
-- Repository  :  https://github.com/dmw/csds
--
-- A Core Stateful Data Structures, PrioQueue based on List Testing module
--
-- This is the initial commit of this project, please write
-- me directly if you want to contribute.
-----------------------------------------------------------------------------

module Data.CSDS.PrioQueueLSpec (spec) where


import Data.Monoid
import Data.CSDS.PrioQueueL
import Test.Hspec


testPushAndPop :: Bool
testPushAndPop = let x :: PrioQueueLT Int
                     x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                     (y, z) = popQ x
                 in toListQ x == [5, 10, 20]
                        && toListQ z == [10, 20]
                               && y == 5


testReverseAndSortPop :: Bool
testReverseAndSortPop = let x :: PrioQueueLT Int
                            x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                            y = sortQ x
                            z = reverseQ y
                        in toListQ y == [5, 10, 20]
                               && toListQ z == [20, 10, 5]


testMonoidMappend :: Bool
testMonoidMappend = let x :: PrioQueueLT Int
                        y :: PrioQueueLT Int
                        x = pushQ 1 $ pushQ 2 $ pushQ 3 emptyQ
                        y = pushQ 4 $ pushQ 5 $ pushQ 6 emptyQ
                        z = x `mappend` y
                        (n, m) = popQ z
                    in toListQ z == [1, 2, 3, 4, 5, 6]
                           && n == 1
                                  && toListQ m == [2, 3, 4, 5, 6]


testMonoidMconcat :: Bool
testMonoidMconcat = let x :: PrioQueueLT Int
                        y :: PrioQueueLT Int
                        x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                        y = pushQ 5 $ pushQ 15 $ pushQ 25 emptyQ
                        r = mconcat [x, y]
                    in toListQ r == [5, 5, 10, 15, 20, 25]


testFoldLQ :: Bool
testFoldLQ = let x :: PrioQueueLT Int
                 x = pushQ 25 $ pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                 r = foldlQ (flip pushQ) x
             in toListQ r == [5, 10, 20, 25]


testMapConcat :: Bool
testMapConcat = let x :: PrioQueueLT Int
                    r :: [PrioQueueLT Int]
                    x = pushQ 25 $ pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                    r = map singletonQ $ toListQ x
                    m = mapConcatQ (+ 5) r
                in toListQ m == [10, 15, 25, 30]


spec :: Spec
spec = do it "check PrioQueueL pushQ and popQ operations" $
             testPushAndPop `shouldBe` True
          it "check PrioQueueL sortQ and reverseQ operations" $
             testReverseAndSortPop `shouldBe` True
          it "check PrioQueueL Monoid mappend operations" $
             testMonoidMappend `shouldBe` True
          it "check PrioQueueL Monoid mconcat operations" $
             testMonoidMconcat `shouldBe` True
          it "check PrioQueueL foldLQ operations" $
             testFoldLQ `shouldBe` True
          it "check PrioQueueL mapConcatQ operations" $
             testMapConcat `shouldBe` True
