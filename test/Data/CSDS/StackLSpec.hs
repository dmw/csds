----------------------------------------------------------------------------
-- |
-- Module      :  Data.CSDS.StackLSpec
-- Copyright   :  (c) Daniel Molina Wegener 2012
-- License     :  BSD 3 (see the LICENSE file)
-- Author      :  Daniel Molina Wegener <dmw@coder.cl>
-- Homepage    :  http://coder.cl/products/csds/
-- Repository  :  https://github.com/dmw/csds
--
-- A Core Stateful Data Structures, Stack based on List Testing module
--
-- This is the initial commit of this project, please write
-- me directly if you want to contribute.
-----------------------------------------------------------------------------

module Data.CSDS.StackLSpec (spec) where


import Data.Monoid
import Data.CSDS.StackL
import Test.Hspec


testPushAndPop :: Bool
testPushAndPop = let x :: StackLT Int
                     x = pushS 20 $ pushS 10 $ pushS 5 emptyS
                     (y, z) = popS x
                 in toListS x == [20, 10, 5]
                        && toListS z == [10, 5]
                               && y == 20


testReverseAndSortPop :: Bool
testReverseAndSortPop = let x :: StackLT Int
                            x = pushS 20 $ pushS 10 $ pushS 5 emptyS
                            y = reverseS x
                            z = sortS x
                 in y == z


testMonoidMappend :: [Int]
testMonoidMappend = let x :: StackLT Int
                        y :: StackLT Int
                        x = pushS 1 $ pushS 2 $ pushS 3 emptyS
                        y = pushS 4 $ pushS 5 $ pushS 6 emptyS
                        z = x `mappend` y
                    in toListS z


testMonoidMconcat :: [Int]
testMonoidMconcat = let x :: StackLT Int
                        y :: StackLT Int
                        x = pushS 1 $ pushS 2 $ pushS 3 emptyS
                        y = pushS 4 $ pushS 5 $ pushS 6 emptyS
                        r = mconcat [x, y]
                    in toListS r


testFoldLS :: Bool
testFoldLS = let x :: StackLT Int
                 x = pushS 25 $ pushS 20 $ pushS 10 $ pushS 5 emptyS
                 r = foldlS (flip pushS) x
             in toListS r == [5, 10, 20, 25]


testFMap :: Bool
testFMap = let x :: StackLT Int
               x = pushS 25 $ pushS 20 $ pushS 10 $ pushS 5 emptyS
               r = fmap (+ 5) x
           in toListS r == [30, 25, 15, 10]


testMapConcat :: [Int]
testMapConcat = let x :: StackLT Int
                    r :: [StackLT Int]
                    x = pushS 1 $ pushS 2 $ pushS 3 $ pushS 4 emptyS
                    r = map singletonS $ toListS x
                    m = mapConcatS (+ 10) r
                in toListS m


addConstMonadic1 :: Int -> Int -> StackLT Int
addConstMonadic1 m x = singletonS (x + 1 - m)


addConstMonadic2 :: Int -> Int -> Int -> StackLT Int
addConstMonadic2 m n x = singletonS (x + m + n)


testMonadic1SL :: [Int]
testMonadic1SL = let x :: StackLT Int
                     r :: [Int]
                     r = [1, 2, 3]
                     x = fromListS r
                         >>= addConstMonadic1 (- 1)
                         >>= addConstMonadic1 1
                         >>= addConstMonadic2 (- 1) 1
                         >>= addConstMonadic2 10 0
                 in toListS x


testMonadic2SL :: [Int]
testMonadic2SL = let x :: StackLT Int
                     r :: [Int]
                     r = [1, 2, 3]
                     x = fromListS r
                         >>= addConstMonadic1 (- 1)
                         >>= addConstMonadic1 1
                         >>= addConstMonadic2 (- 1) 1
                         >>= addConstMonadic2 10 0
                         >> addConstMonadic1 1 1
                         >> addConstMonadic2 1 1 1
                 in toListS x


spec :: Spec
spec = do it "check StackL pushS and popS operations" $
             testPushAndPop `shouldBe` True

          it "check StackL sortS and reverseS operations" $
             testReverseAndSortPop `shouldBe` True

          it "check StackL Monoid mappend operations" $
             testMonoidMappend `shouldBe` [1, 2, 3, 4, 5, 6]

          it "check StackL Monoid mconcat operations" $
             testMonoidMconcat `shouldBe` [1, 2, 3, 4, 5, 6]

          it "check StackL foldLS operations" $
             testFoldLS `shouldBe` True

          it "check StackL fmap operations" $
             testFMap `shouldBe` True

          it "check StackL mapConcatS operations" $
             testMapConcat `shouldBe` [11, 12, 13, 14]

          it "check StackL Monadic >>= operations" $
             testMonadic1SL `shouldBe` [13, 14, 15]

          it "check StackL Monadic >> operations" $
             testMonadic2SL `shouldBe` [3]
