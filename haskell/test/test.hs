module Main
where

import Afgame
import Afgame.Internal
import Test.Framework (testGroup, defaultMain)
import Test.Framework.Providers.HUnit
import Control.Applicative
import Test.HUnit hiding (test)

-- let's make some easy shots
strike = (Strike, 15)
spare = (Spare, 10)
normal = (Normal, 5)

main :: IO ()
main = defaultMain tests

testNormal :: IO()
testNormal = scores (replicate 15 5) emptyBoard @?= Right (Done 75)

testLastFrameNormal = do
    let s = scores [15, 15, 15, 15, 5] emptyBoard
    s @?= Right (Current (170,[[(Normal,5)],[(Strike,15)],[(Strike,15)],[(Strike,15)],[(Strike,15)]]))

testOneStrike = do
    score 15 emptyBoard @?= Right (Current (15,[[strike]]))

testStrikeScore = do
    let s = scores [15, 5, 5, 5, 5] emptyBoard
    s @?= Right (Current (50,[[(Normal,5)],[(Normal,5),(Normal,5),(Normal,5)],[(Strike,15)]]))

testLastFrameStrike = do
    let s = scores [15, 5, 5, 5] [[strike], [strike], [strike], [strike]]
    s @?= Right (Done 255)

testOneSpare = do
    scores [5, 10] emptyBoard @?= Right (Current (15, [[spare, normal]]))

testSpareScore = do
    let s = scores [5, 10, 5, 5, 5] emptyBoard
    s @?= Right (Current (40,[[(Normal,5),(Normal,5),(Normal,5)],[(Spare,10),(Normal,5)]]))

testLastFrameSpare = do
    let s = scores [5, 10, 5, 5] [[strike], [strike], [strike], [strike]]
    s @?= Right (Done 225)

testOverLongerLastFrame_1 =
    isLastFrameOver [strike, normal, normal]
    @?=
    False

testOverLongerLastFrame_2 =
    isLastFrameOver [normal, spare, normal]
    @?=
    False

testOverNormalLastFrame_1 =
    isLastFrameOver [normal, normal, normal]
    @?=
    True

testOverNormalLastFrame_2 =
    isLastFrameOver [spare, normal, normal]
    @?=
    True

tests = [
    testGroup "Normal"
        [ testCase "Update frames with 15 normal shots" testNormal
        , testCase "one normal shots on the last frames" testLastFrameNormal
        ],
    testGroup "Strike"
        [ testCase "Just one Strike" testOneStrike
        , testCase "The 3 next shots are added to the score of one strike" testStrikeScore
        , testCase "Last frame is special" testLastFrameStrike
        ],
    testGroup "Spare"
        [ testCase "Just one Spare" testOneSpare
        , testCase "The 2 next shots are added to the score of a spare" testSpareScore
        , testCase "The 2 last shots are added to the score of the spare" testLastFrameSpare
        ],
    testGroup "LastFrame"
        [ testCase "We should allow 4 shots in this frame" testOverLongerLastFrame_1
        , testCase "We should allow only 3 shots in this frame" testOverLongerLastFrame_2
        , testCase "We should allow only 3 shots in this frame" testOverNormalLastFrame_1
        , testCase "We should allow only 3 shots with only one spare at the first position" testOverNormalLastFrame_2
        ]
     ]
