module Afgame.MVC

where

import           Afgame
import           Afgame.Internal

import           Control.Applicative
import           Control.Monad.Identity
import qualified Control.Monad.State.Strict as S
import           Lens.Family.Stock (_Left, _Right)  -- from `lens-family-core`
import           MVC
import qualified MVC.Prelude as MVC
import qualified Pipes.Prelude as P
import           Test.QuickCheck

main :: IO Board
main =  runMVC emptyBoard model vc

-- | managed controller
controller :: Managed (Controller String)
controller = join $ managed $ \k -> do
    putStrLn "Let's start Booling ! Enter your shots.\n(enter 'q' to quit)"
    k MVC.stdinLines

{- separate views for rendering messages and scores

Either is hijacked with Left for messages and Right for scores

-}
viewMessage :: View String
viewMessage = asSink (putStrLn . ("message: " ++))

viewScore :: View ScoreBoard
viewScore = asSink (putStrLn . ("score: " ++) . show)

view :: View (Either String ScoreBoard)
view = handles _Left viewMessage <> handles _Right viewScore

-- | matching the type needed for runMVC
vc :: Managed (View (Either String ScoreBoard), Controller String)
vc = do
    c <- controller
    return (view, c)

{- models which separates the process model into quitting, reading and score calculation

Previously, reading from stdin mashes together the concept of reading a 'q' and reading an Int.  Anything else was silently failing.

To expose this, the model pipe is fed a String, looks for a 'q', then attempts to read the String to an Int.  An Either String Int is then used to pass the message along.

An alternative to this would be to split the controller into a 'q' controller and an Int reader controller (eg if you dont case about sending messages if the input isnt a number).

-}

pipeQuit :: (Monad m) => Pipe String String m ()
pipeQuit = P.takeWhile (/= "q")

pipeRead :: (Monad m) => Pipe String (Either String Int) m ()
pipeRead = for cat $ \str -> case reads str of
    [(a,"")] -> yield $ Right a
    _ -> yield $ Left "Enter a number, or 'q' to quit"

{-
It would be much better code if the type signature was Pipe Int (Either String (Int,Board) (and pretty much identical with the original score pipe) rather than including the Left messages from the previous step.  A ListT appraoch might help here.

-}

pipeScore :: (Monad m) => Board -> Pipe (Either String Int) (Either String ScoreBoard) m ()
pipeScore =  go
    where
        go b =  do
            x <- await
            case x of
                Left s -> yield (Left s)
                Right n -> do
                    case score n b of
                        Right c@(Current (_, b')) -> do
                            yield (Right c)
                            go b'
                        Right (Done s) ->
                            yield $ Right (Done s)
                        Left msg -> do
                            yield (Left msg)
                            go b

model :: Model Board String (Either String ScoreBoard)
model = asPipe (pipeQuit >-> pipeRead >-> pipeScore emptyBoard)


{- modelling pure code
The best part of mvc is about modelling and testing the pure logic of the game.

-}

-- we need some Ints to represent moves
someInts :: IO [Int]
someInts = generate (infiniteList :: Gen [Int])

-- how many steps does a game take?
calcGameSteps :: Int -> [Int] -> Int
calcGameSteps m ns =
    -- runIdentity can be removed if you pick the State monad in place of StateT
    runIdentity $ flip S.evalStateT emptyBoard $ P.length $
        each ns >->
        P.map Right >->
        pipeScore emptyBoard >-> -- the pure scoring element of the model
        P.take m >->  -- just in case it's not turing complete
        P.filter (\x -> case x of (Right _) -> True; (Left _) -> False) -- only count valid moves

{-
λ> calcGameSteps 1000 <$> someInts
15
λ> calcGameSteps 1000 <$> someInts
16
λ> calcGameSteps 1000 <$> someInts
13

It never takes more than 16 moves ?
-}

prop_gamestepsGT16 :: [Int] -> Bool
prop_gamestepsGT16 xs =
    16 >= calcGameSteps 1000 xs

{-
λ> quickCheck prop_gamestepsGT16
-- +++ OK, passed 100 tests.
-}

{- scoring -}
calcFinalScore ::  [Int] -> Maybe Int
calcFinalScore ns =
    runIdentity $ flip S.evalStateT emptyBoard $ P.last $
        each ns >->
        P.map Right >->
        pipeScore emptyBoard >->
        getScore
      where
        getScore = do
            a <- await
            case a of
                Right (Done s) -> yield s
                _ -> getScore


prop_alwaysscore :: [Int] -> Property
prop_alwaysscore xs = not (null xs) ==>
    Nothing /= calcFinalScore xs

{-
λ> quickCheck prop_alwaysscore
-- +++ OK, passed 100 tests.
-}

prop_highscore :: Int -> [Int] -> Property
prop_highscore high xs = not (null xs) ==>
    let s = calcFinalScore xs in
    case s of
        Nothing -> True
        Just x -> high >= x

{-
λ> quickCheck (prop_highscore 17)
Falsifiable (after 16 tests and 6 shrinks):
[0,0,4,14]
λ>

-- 40

Falsifiable (after 215 tests and 9 shrinks):
[0,0,2,12,1,0,8,-1,8,11]

scores above 40 seem to be rare

At this stage, I'm going to guess that moves need to be between 1 and 15, and restrict quick check to this range.

-}


newtype GameMove = GameMove Int deriving (Show, Eq, Ord)

instance Arbitrary GameMove where
    arbitrary = GameMove <$> choose (1, 15)

prop_highscore' :: Int -> [GameMove] -> Property
prop_highscore' high xs = not (null xs) ==>
    let s = calcFinalScore (map (\(GameMove x) -> x) xs) in
    case s of
        Nothing -> True
        Just x -> high >= x

{-
λ> quickCheckWith stdArgs{maxSuccess=1000} (prop_highscore' 150)

Falsifiable (after 94 tests and 13 shrinks):
[GameMove 12,GameMove 3,GameMove 13,GameMove 2,GameMove 3,GameMove 1,GameMove 8,GameMove 9,GameMove 6,GameMove 15,GameMove 11,GameMove 6]

-- 180

[GameMove 15,GameMove 15,GameMove 14,GameMove 1,GameMove 15,GameMove 14,GameMove 14]

-- 200

[GameMove 10,GameMove 5,GameMove 6,GameMove 5,GameMove 3,GameMove 13,GameMove 2,GameMove 15,GameMove 15,GameMove 13,GameMove 12]

-- 220

+++ OK, passed 1000 tests.

highest scores are no capping out around about 200.

I still have no idea about the actual rules of the game but the shrunk examples of how to get a high score suggest 15 as a good choice, and consecutive moves that add to 15.

Cribbage!


-}
