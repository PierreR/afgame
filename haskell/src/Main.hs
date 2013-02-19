module Main
where

import Control.Monad.State
import Afgame
import Test.Framework (testGroup, defaultMain)
import Test.Framework.Providers.HUnit

import Test.HUnit hiding (test)

-- let's make some easy shots
strike = (Strike, 15)
spare = (Spare, 10)
normal = (Normal, 5)


main :: IO ()
main = defaultMain tests

testNormal =
	runStateT (sequence $ replicate (5*3) $ shot 5) [[]]
	@?=
	Just (take 15 $ [5, 10 ..], replicate 5 $ replicate 3 normal)

testLastFrameNormal =
	runStateT (sequence [shot 5]) [[strike], [strike], [strike], [strike]]
	@?=
	Just ([(60+50+35+20+5)], [ [normal], [strike],[strike], [strike], [strike] ])

testGameOver =
	runStateT (shot 5) [[strike, strike, strike, strike], [strike], [strike], [strike], [strike]]
	@?=
	Nothing

testOneStrike =
	runStateT (shot 15) [[]]
	@?=
	Just (15, [[strike]])

testStrikeScore =
	runStateT (sequence [shot 15, shot 5, shot 5, shot 5, shot 5]) [[]]
	@?=
	Just ([15,25,35,45,50], [[normal], [normal, normal, normal], [strike]])

testLastFrameStrike =
	runStateT (sequence [shot 15, shot 5, shot 5, shot 5]) [[strike], [strike], [strike], [strike]]
	@?=
	Just ([210, 230, 245, 255], [[normal, normal, normal, strike], [strike],[strike], [strike], [strike]])

testOneSpare =
	runStateT (sequence [shot 5, shot 10]) [[]]
	@?=
	Just ([5, 15], [[spare, normal]])

testSpareScore =
	runStateT (sequence [shot 5, shot 10, shot 5, shot 5, shot 5]) [[]]
	@?=
	Just ([5, 15, 25, 35, 40], [[normal, normal, normal ], [spare, normal]])

testLastFrameSpare =
	runStateT (sequence [shot 5, shot 10, shot 5, shot 5]) [[strike], [strike], [strike], [strike]]
	@?=
	Just ([170, 200, 215, (215 + 5 + 5)], [[normal, normal, spare, normal], [strike],[strike], [strike], [strike]])

testLastFrameSpecialOver_1 =
	isLastFrameOver [strike, normal, normal]
	@?=
        False

testLastFrameSpecialOver_2 =
	isLastFrameOver [normal, spare, normal]
	@?=
        False

testLastFrameNormalOver_1 =
	isLastFrameOver [normal, normal, normal]
	@?=
	True

testLastFrameNormalOver_2 =
	isLastFrameOver [spare, normal, normal]
	@?=
	True

tests =
    [ testGroup "Normal"
    	[ testCase "Update frames with 15 normal shots" testNormal
    	, testCase "one normal shots on the last frames" testLastFrameNormal
    	, testCase "Game should terminate" testGameOver
    	]
    , testGroup "Strike"
    	[ testCase "Just one Strike" testOneStrike
    	, testCase "The 3 next shots are added to the score of one strike" testStrikeScore
    	, testCase "Last frame is special" testLastFrameStrike
    	]
    , testGroup "Spare"
    	[ testCase "Just one Spare" testOneSpare
    	, testCase "The 2 next shots are added to the score of a spare" testSpareScore
    	, testCase "The 2 last shots are added to the score of the spare" testLastFrameSpare
    	]
    , testGroup "LastFrame"
    	[ testCase "We should allow 4 shots in this frame" testLastFrameSpecialOver_1
    	, testCase "We should allow only 3 shots in this frame" testLastFrameSpecialOver_2
    	, testCase "We should allow only 3 shots in this frame" testLastFrameNormalOver_1
    	, testCase "We should allow only 3 shots with only one spare at the first position" testLastFrameNormalOver_2
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
