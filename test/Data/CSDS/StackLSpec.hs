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


import Data.Monoid ()
import Data.CSDS.StackL
import Test.Hspec


testPushAndPop :: Bool
testPushAndPop = let x :: StackLT Int
                     x = pushS 20 $ pushS 10 $ pushS 5 emptyS
                     (y, z) = popS x
                 in (toListS x) == [20, 10, 5]
                        && (toListS z) == [10, 5]
                               && y == 20

testReverseAndSortPop :: Bool
testReverseAndSortPop = let x :: StackLT Int
                            x = pushS 20 $ pushS 10 $ pushS 5 emptyS
                            y = reverseS x
                            z = sortS x
                 in y == z

spec :: Spec
spec = do it "check StackL push and pop operations" $
             testPushAndPop `shouldBe` True
          it "check StackL sort and reverse operations" $
             testReverseAndSortPop `shouldBe` True


