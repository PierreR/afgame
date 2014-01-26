module Main
where

import Afgame
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

testNormal = do
    s <-  P.toListM $ score (each $ replicate 15 5) >-> P.map fst
    s @?= take 15 [5, 10 ..]

testLastFrameNormal = do
    s <-  P.toListM $ score (each $ [15, 15, 15, 15, 5]) >-> P.map fst
    s @?= [15,45,90,150,170]

testOneStrike = do
    s <- P.toListM (score $ yield 15)
    s @?= [(15,[[strike]])]

testStrikeScore = do
    s <- last <$> P.toListM (score $ each [15, 5, 5, 5, 5])
    s @?= (50,[[normal],[normal, normal, normal],[strike]])

testLastFrameStrike = do
    s <- P.toListM (evalStateP [[strike], [strike], [strike], [strike]] (parseShot $ each [15, 5, 5, 5]) >-> P.map fst)
    s @?= [210, 230, 245, 255]

testOneSpare = do
    s <- P.toListM (score $ each [5, 10])
    s @?= [(5, [[normal]]), (15, [[spare, normal]])]

testSpareScore = do
    s <- P.toListM $ score (each [5, 10, 5, 5, 5]) >-> P.map fst
    s @?= [5, 15, 25, 35, 40]

testLastFrameSpare = do
    s <- P.toListM (evalStateP [[strike], [strike], [strike], [strike]] (parseShot $ each [5, 10, 5, 5]) >-> P.map fst)
    s @?= [170, 200, 215, (215 + 5 + 5)]

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

