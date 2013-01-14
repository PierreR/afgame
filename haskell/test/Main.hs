module Main
where

import Data.List
import Afgame
import Test.Framework (testGroup, defaultMain)
import Test.Framework.Providers.HUnit 

import Test.HUnit hiding (test)


main :: IO ()
main = defaultMain tests

testSimple :: Assertion
testSimple = updateBoard 15 [[]] @?= (30, [[], [15]])


tests =
    [ testGroup "HUnit tests"
    	[ testCase "simple" testSimple
    	]
    ]
