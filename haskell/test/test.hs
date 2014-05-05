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
testNormal = do
    let s = fst <$> scores (replicate 15 5) emptyBoard
    s @?= Right (take 15 [5, 10 ..])

testLastFrameNormal = do
    let s = fst <$> scores [15, 15, 15, 15, 5] emptyBoard
    s @?= Right [15,45,90,150,170]

testOneStrike = do
    score 15 emptyBoard @?= Right (15,[[strike]])

testStrikeScore = do
    let s = scores [15, 5, 5, 5, 5] emptyBoard
    s @?= Right ([15,25,35,45,50],[[normal],[normal, normal, normal],[strike]])

testLastFrameStrike = do
    let s = fst <$> scores [15, 5, 5, 5] [[strike], [strike], [strike], [strike]]
    s @?= Right [210, 230, 245, 255]

testOneSpare = do
    (scores) [5, 10] emptyBoard @?= Right ([5,15], [[spare, normal]])

testSpareScore = do
    let s = fst <$> scores [5, 10, 5, 5, 5] emptyBoard
    s @?= Right [5, 15, 25, 35, 40]

testLastFrameSpare = do
    let s = fst <$> scores [5, 10, 5, 5] [[strike], [strike], [strike], [strike]]
    s @?= Right [170, 200, 215, (215 + 5 + 5)]

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
