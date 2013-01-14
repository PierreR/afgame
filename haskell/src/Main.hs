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
	runState (sequence $ replicate (5*3) $ shot 5) [[]]
	@?= 
	(take 15 [5,10..], replicate 5 $ replicate 3 normal)

testLastFrameNormal =
	runState (sequence [shot 5]) [[strike], [strike], [strike], [strike]]
	@?=
	([(60+50+35+20+5)], [ [normal], [strike],[strike], [strike], [strike] ])

testOneStrike = 
	runState (shot 15) [[]] 
	@?= 
	(15, [[strike]])

testStrikeScore =
	runState (sequence [shot 15, shot 5, shot 5, shot 5, shot 5]) [[]]
	@?=
	([15, 25, 35, 45, 50], [[normal], [normal, normal, normal], [strike]])

testLastFrameStrike =
	runState (sequence [shot 15, shot 5, shot 5, shot 5]) [[strike], [strike], [strike], [strike]]
	@?=
	([210, 230, 245, 255], [[normal, normal, normal, strike], [strike],[strike], [strike], [strike]])

testOneSpare =
	runState (sequence [shot 5, shot 10]) [[]]
	@?=
	([5, 15], [[spare, normal]])

testSpareScore =
	runState (sequence [shot 5, shot 10, shot 5, shot 5, shot 5]) [[]]
	@?=
	([5, 15, 25, 35, 40], [[normal, normal, normal ], [spare, normal]])

testLastFrameSpare =
	runState (sequence [shot 5, shot 10, shot 5, shot 5]) [[strike], [strike], [strike], [strike]]
	@?=
	([170, 200, 215, (215 + 5 + 5)], [[normal, normal, spare, normal], [strike],[strike], [strike], [strike]])

tests =
    [ testGroup "Normal"
    	[ testCase "Update frames with 15 normal shots" testNormal
    	, testCase "one normal shots on the last frames" testLastFrameNormal
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
    ]


--module Main (main)
--where

--import Afgame
--import Control.Monad.State

---- Let's shot
--shot :: Int -> State Board Int
--shot a = state $ updateBoard a

--main :: IO()
--main = print $ runState (sequence [shot 15, shot 15]) [[]]