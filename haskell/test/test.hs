module Main
where

import Pipes
import Pipes.Lift
import qualified Pipes.Prelude as P
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

--testGameOver =
--    runStateT (shot 5) [[strike, strike, strike, strike], [strike], [strike], [strike], [strike]]
--    @?=
--    Nothing

testOneStrike = do
    s <- P.toListM $ (score $ yield 15)
    s @?= [(15,[[strike]])]

--testStrikeScore =
--    runStateT (sequence [shot 15, shot 5, shot 5, shot 5, shot 5]) [[]]
--    @?=
--    Just ([15,25,35,45,50], [[normal], [normal, normal, normal], [strike]])

--testLastFrameStrike =
--    runStateT (sequence [shot 15, shot 5, shot 5, shot 5]) [[strike], [strike], [strike], [strike]]
--    @?=
--    Just ([210, 230, 245, 255], [[normal, normal, normal, strike], [strike],[strike], [strike], [strike]])

--testOneSpare =
--    runStateT (sequence [shot 5, shot 10]) [[]]
--    @?=
--    Just ([5, 15], [[spare, normal]])

--testSpareScore =
--    runStateT (sequence [shot 5, shot 10, shot 5, shot 5, shot 5]) [[]]
--    @?=
--    Just ([5, 15, 25, 35, 40], [[normal, normal, normal ], [spare, normal]])

--testLastFrameSpare =
--    runStateT (sequence [shot 5, shot 10, shot 5, shot 5]) [[strike], [strike], [strike], [strike]]
--    @?=
--    Just ([170, 200, 215, (215 + 5 + 5)], [[normal, normal, spare, normal], [strike],[strike], [strike], [strike]])

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
    --    --, testCase "Game should terminate" testGameOver
        ],
    testGroup "Strike"
       [ testCase "Just one Strike" testOneStrike
    --    , testCase "The 3 next shots are added to the score of one strike" testStrikeScore
    --    , testCase "Last frame is special" testLastFrameStrike
       ],
    --, testGroup "Spare"
    --    [ testCase "Just one Spare" testOneSpare
    --    , testCase "The 2 next shots are added to the score of a spare" testSpareScore
    --    , testCase "The 2 last shots are added to the score of the spare" testLastFrameSpare
    --    ]
    testGroup "LastFrame"
        [ testCase "We should allow 4 shots in this frame" testOverLongerLastFrame_1
        , testCase "We should allow only 3 shots in this frame" testOverLongerLastFrame_2
        , testCase "We should allow only 3 shots in this frame" testOverNormalLastFrame_1
        , testCase "We should allow only 3 shots with only one spare at the first position" testOverNormalLastFrame_2
        ]
     ]


--module Main (main)
--where

--import Afgame
--import Control.Monad.State

---- Let's shot
--shot :: Int -> State Board Int
--shot a = state $ updateBoard a

--main :: IO()
--main = print $ runStateT (sequence [shot 15, shot 15]) [[]]
