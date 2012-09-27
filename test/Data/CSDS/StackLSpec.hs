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


testMonoidMappend :: Bool
testMonoidMappend = let x :: StackLT Int
                        y :: StackLT Int
                        l :: [Int]
                        x = pushS 20 $ pushS 10 $ pushS 5 emptyS
                        y = pushS 5 $ pushS 15 $ pushS 25 emptyS
                        z = x `mappend` y
                        l = [20, 10, 5, 5, 15, 25]
                    in toListS z == l


testMonoidMconcat :: Bool
testMonoidMconcat = let x :: StackLT Int
                        y :: StackLT Int
                        x = pushS 20 $ pushS 10 $ pushS 5 emptyS
                        y = pushS 5 $ pushS 15 $ pushS 25 emptyS
                        z = x `mappend` y
                        r = mconcat [x, y]
                    in toListS r == toListS z


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


testMapConcat :: Bool
testMapConcat = let x :: StackLT Int
                    r :: [StackLT Int]
                    x = pushS 25 $ pushS 20 $ pushS 10 $ pushS 5 emptyS
                    r = map singletonS $ toListS x
                    m = mapConcatS (+ 5) r
                    n = fmap (+ 5) x
                in toListS n == toListS m


addConstMonadic1 :: Int -> Int -> StackLT Int
addConstMonadic1 m x = singletonS (x + 5 + m)


addConstMonadic2 :: Int -> Int -> Int -> StackLT Int
addConstMonadic2 m n x = singletonS (x + m + n)


testMonadicSL :: Bool
testMonadicSL = let x :: StackLT Int
                    r :: [Int]
                    r = [10, 20, 30]
                    x = fromListS r
                        >>= addConstMonadic1 10
                        >>= addConstMonadic1 20
                        >>= addConstMonadic2 (- 10) 25
                in toListS x == [65, 75, 85]


spec :: Spec
spec = do it "check StackL pushS and popS operations" $
             testPushAndPop `shouldBe` True
          it "check StackL sortS and reverseS operations" $
             testReverseAndSortPop `shouldBe` True
          it "check StackL Monoid mappend operations" $
             testMonoidMappend `shouldBe` True
          it "check StackL Monoid mconcat operations" $
             testMonoidMconcat `shouldBe` True
          it "check StackL foldLS operations" $
             testFoldLS `shouldBe` True
          it "check StackL fmap operations" $
             testFMap `shouldBe` True
          it "check StackL mapConcatS operations" $
             testMapConcat `shouldBe` True
          it "check StackL Monadic operations" $
             testMonadicSL `shouldBe` True
