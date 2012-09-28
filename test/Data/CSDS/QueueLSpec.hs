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


testReversePop :: [Int]
testReversePop = let x :: QueueLT Int
                     x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                     y = reverseQ x
                 in toListQ y


testSortPop :: [Int]
testSortPop = let x :: QueueLT Int
                  x = pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                  z = sortQ x
              in toListQ z


testMonoidMappend :: [Int]
testMonoidMappend = let x :: QueueLT Int
                        y :: QueueLT Int
                        x = pushQ 3 $ pushQ 2 $ pushQ 1 emptyQ
                        y = pushQ 6 $ pushQ 5 $ pushQ 4 emptyQ
                        z = x `mappend` y
                    in toListQ z


testMonoidMconcat :: [Int]
testMonoidMconcat = let x :: QueueLT Int
                        y :: QueueLT Int
                        x = pushQ 3 $ pushQ 2 $ pushQ 1 emptyQ
                        y = pushQ 6 $ pushQ 5 $ pushQ 4 emptyQ
                        r = mconcat [x, y]
                    in toListQ r


testFoldLQ :: Bool
testFoldLQ = let x :: QueueLT Int
                 x = pushQ 25 $ pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
                 r = foldlQ (flip pushQ) x
             in toListQ r == [5, 10, 20, 25]


testFMap :: Bool
testFMap = let x :: QueueLT Int
               x = pushQ 25 $ pushQ 20 $ pushQ 10 $ pushQ 5 emptyQ
               r = fmap (+ 5) x
           in toListQ r == [10, 15, 25, 30]


testMapConcat :: [Int]
testMapConcat = let x :: QueueLT Int
                    r :: [QueueLT Int]
                    x = pushQ 1 $ pushQ 2 $ pushQ 3 $ pushQ 4 emptyQ
                    r = map singletonQ $ toListQ x
                    m = mapConcatQ (+ 10) r
                in toListQ m


addConstMonadic1 :: Int -> Int -> QueueLT Int
addConstMonadic1 m x = singletonQ (x + 1 - m)


addConstMonadic2 :: Int -> Int -> Int -> QueueLT Int
addConstMonadic2 m n x = singletonQ (x + m + n)


testMonadic1SL :: [Int]
testMonadic1SL = let x :: QueueLT Int
                     r :: [Int]
                     r = [1, 2, 3]
                     x = fromListQ r
                         >>= addConstMonadic1 (- 1)
                         >>= addConstMonadic1 1
                         >>= addConstMonadic2 (- 1) 1
                         >>= addConstMonadic2 10 0
                 in toListQ x


testMonadic2SL :: [Int]
testMonadic2SL = let x :: QueueLT Int
                     r :: [Int]
                     r = [1, 2, 3]
                     x = fromListQ r
                         >>= addConstMonadic1 (- 1)
                         >>= addConstMonadic1 1
                         >>= addConstMonadic2 (- 1) 1
                         >>= addConstMonadic2 10 0
                         >> addConstMonadic1 1 1
                         >> addConstMonadic2 1 1 1
                 in toListQ x


spec :: Spec
spec = do it "check QueueL pushQ and popQ operations" $
             testPushAndPop `shouldBe` True

          it "check QueueL reverseQ operations" $
             testReversePop `shouldBe` [20, 10, 5]

          it "check QueueL sortQ operations" $
             testSortPop `shouldBe` [5, 10, 20]

          it "check QueueL Monoid mappend operations" $
             testMonoidMappend `shouldBe` [1, 2, 3, 4, 5, 6]

          it "check QueueL Monoid mconcat operations" $
             testMonoidMconcat `shouldBe` [1, 2, 3, 4, 5, 6]

          it "check QueueL foldLQ operations" $
             testFoldLQ `shouldBe` True

          it "check QueueL fmap operations" $
             testFMap `shouldBe` True

          it "check QueueL mapConcatQ operations" $
             testMapConcat `shouldBe` [14, 13, 12, 11]

          it "check QueueL Monadic >>= operations" $
             testMonadic1SL `shouldBe` [13, 14, 15]

          it "check QueueL Monadic >> operations" $
             testMonadic2SL `shouldBe` [3]
