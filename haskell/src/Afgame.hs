
------------------------------------------------------------------------------
-- | This module calculates the score of the African game.
-- See <https://github.com/PierreR/afgame/README.md> for the specifications.
-- It uses a state monad to hold all 'current scores' (the value) together with
-- the status of the game (called the board).
-- One important note :
-- According to the definition of the game, a strike or spare earns the point of
-- the next three or two shots. This implementation just adds the "after" shots
-- (the pins knocked down), not the real score obtained by these shots.

module Afgame (score, Hit(..), isLastFrameOver)
where

import Pipes
import Pipes.Lift
import qualified Control.Monad.State.Strict as S

import Control.Applicative

import Control.Arrow ((>>>))

import qualified Data.Sequence as Seq
import Text.Printf

-------------------------------------------------------------------------------
--
-- In case of a strike or spare, the score of one shot will depend on the next
-- shots. Consequently there is not point to calculate the score while we update
-- the board and this implementation separate the two concerns.
------------------------------------------------------------------------------

-- | A shot is defined by a Hit together with the number of pins knocked down.
type Shot =  (Hit, Int)

-- | A shot can be a Strike, a Spare or just Normal.
data Hit = Strike | Spare | Normal deriving (Eq,Show)

-- | Shots happens within Frames
-- A frame is a list of shots.
type Frame = [Shot]

-- | There are 15 bowling pins to knock down
allPins :: Int
allPins = 15

-- | the game breaks down into 5 frames
maxFrame :: Int
maxFrame = 5

-- | Within a Frame, from the number of pins down, construct a 'Shot'.
newShot :: Int -> Frame -> Shot
newShot a  f
    | isStrike a   = (Strike, allPins)
    | isSpare a f  = (Spare, a)
    | otherwise    = (Normal, a)

-- | Strike when all the pins are knocked down
isStrike :: Int -> Bool
isStrike a = a == allPins

-- | Spare when
--     1. the frame is not empty
--     2. all pins are knocked down within 2 shots
--     3. the previous shot is not a spare (only happens in the last frame)
isSpare :: Int -> Frame -> Bool
isSpare a f =
    let (previousHit, previousShot) = head f
    in  not (null f)
        && a + previousShot == allPins
        && previousHit /= Spare

isShotBogus :: Int -> Bool
isShotBogus a = a > allPins

-- | A board composed of frames hold the whole game state.
type Board = [Frame]

emptyBoard :: Board
emptyBoard = [[]]

newBoard :: Int -> Board -> Board
newBoard _ [] = error "Board game should be initialized"
newBoard a b@(currentFrame:xs) = 
    updateFrame a currentFrame b ++ xs

-- | The forth frame is always the last frame.
-- PS: '(>>>)' is defined as 'flip (.)' for '(->)'
isLastFrame :: Board -> Bool
isLastFrame = length  >>> (>= maxFrame)

-- | A frame is composed of maximum 3 shots and is over whenever all pins are down.
-- The rule is different for the last frame.
isFrameOver :: Board -> Bool
isFrameOver b
    | isLastFrame b = isLastFrameOver f
    | otherwise     = sumShots f >= allPins || length f >= 3
    where f = head b -- take the current frame

-- | The last frame is not over in case of a strike or spare
-- It is composed of 3 to 4 shots depending on the need to account for strike/spare.
isLastFrameOver :: Frame -> Bool
isLastFrameOver f
    | isLonger   = length f >= 4
    | otherwise  = length f >= 3
    -- To have a longer frame:
    -- one strike within the frame is enough
    -- one spare is usually enough except if it is at the first position
    where isLonger  =
            Seq.foldrWithIndex isSpecial False (Seq.fromList f)
            where isSpecial i (h,_) acc = acc || (h == Strike || (i /= 0 && h == Spare))

-- | The game is over at the last frame when the frame is over
isGameOver :: Board -> Bool
isGameOver = (&&) <$> isLastFrame <*> isFrameOver

-- A normal frame is bogus if the shots add up for more than 15
isFrameValid :: Board -> Bool
isFrameValid = (||) <$> isLastFrame <*> (head >>> sumShots >>> (<= allPins))

-- | From the current frame, either
--      - push the new shot in the current frame
--   or - create a new frame containing the sole new shot.
-- Return as a list of one or two frame(s).
updateFrame :: Int -> Frame -> Board -> [Frame]
updateFrame a f b
    | isFrameOver b   = [[new], f]  -- create a new frame with one new shot
    | otherwise       = [ new : f]  -- push the new shot in the current frame
    where new = newShot a f

-- | Calculate the score of the updated board.
-- First we flatten (concat) the board to remove frame information
-- Then we calc each score chronological wise.
-- Indexing allows to look into the future to account for Strikes & Spares
calcScore :: Board -> Int
calcScore b =
    -- reverse to get all shots in chronological order
    -- it makes it easier to reason about what is "after" one indexed shot
    let allShots = (reverse . concat) b
        indexedShots = Seq.fromList allShots
    in Seq.foldlWithIndex (calcShot allShots) 0 indexedShots
    where
        calcShot :: [Shot] -> Int -> Int -> (Hit, Int) -> Int
        calcShot xs acc i (h,a)
            | h == Strike = accScore + sumShots (take 3 after) -- Strike earns the score of the 3 next shots
            | h == Spare  = accScore + sumShots (take 2 after) -- Spare earns the score of the 2 next shots
            | h == Normal = accScore
            | otherwise = 0
            where
                accScore = acc + a -- sum the new score in the fold accumulator
                after = drop (succ i) xs -- timewise, list of shots recorded after the current index


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

sumShots :: Frame -> Int
sumShots = sum . map snd


--------------------------------------------------------------------------------
-- Pipes
--------------------------------------------------------------------------------

filterShot :: Pipe Int Int IO ()
filterShot = for cat $ \a -> 
    if isShotBogus a 
        then liftIO $ putStrLn "Bogus shot"
        else yield a

parseShot :: Pipe Int (Board, Int) (S.StateT Board IO) ()
parseShot = go
    where
        go = do
            shot <- await
            b <- lift S.get
            let b' = newBoard shot b
            if isFrameValid b'
                then do
                    lift $ S.put b'
                    yield (b', calcScore b')
                else liftIO $ putStrLn "This shot creates an invalid frame. Shot ignored. Go on"
            if isGameOver b' then liftIO $ putStrLn "The game is over" else go

pprint :: Pipe (Board, Int) String IO ()
pprint = for cat $ \(b, a) -> do  
    yield $ printf "Score is %d. Board is %s." a (show b)

-- | Feed with Shot, `score` streams a display of the score as a `String`        
score :: Pipe Int String IO ()
score = filterShot >-> evalStateP emptyBoard parseShot >-> pprint


