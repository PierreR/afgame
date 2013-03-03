------------------------------------------------------------------------------
-- | This module calculates the score of the African game.
-- See <https://github.com/PierreR/afgame/README.md> for the specifications.
-- It uses a state monad to hold all 'current scores' (the value) together with
-- the status of the game (called the board).
-- One important note :
-- According to the definition of the game, a strike or spare earns the point of
-- the next three or two shots. This implementation just adds the "after" shots
-- (the pins knocked down), not the real score obtained by these shots.

module Afgame (shot, Hit(..), isLastFrameOver)
where

import Control.Monad.State

import qualified Data.Sequence as S

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
all_pins = 15

-- | the game breaks down into 5 frames
max_frame = 5

-- | Within a Frame, from the number of pins down, construct a 'Shot'.
newShot :: Int -> Frame -> Shot
newShot a  f
    | isStrike a   = (Strike, all_pins)
    | isSpare a f  = (Spare, a)
    | otherwise    = (Normal, a)

-- | Strike when all the pins are knocked down
isStrike :: Int -> Bool
isStrike a = a == all_pins

-- | Spare when
--     1. the frame is not empty
--     2. all pins are knocked down within 2 shots
--     3. the previous shot is not a spare (only happens in the last frame)
isSpare :: Int -> Frame -> Bool
isSpare a f =
    let (previousHit, previousShot) = head f
    in  not (null f)
        && a + previousShot == all_pins
        && previousHit /= Spare

-- | A board composed of frames hold the whole game state.
type Board = [Frame]

-- | The forth frame is always the last frame.
isLastFrame :: Board -> Bool
isLastFrame b = length b >= max_frame

-- | A frame is composed of maximum 3 shots and is over whenever all pins are down.
-- The rule is different for the last frame.
isFrameOver :: Board -> Bool
isFrameOver b
    | isLastFrame b = isLastFrameOver f
    | otherwise     = sumShots f >= all_pins || length f >= 3
    where f = head b -- take the current frame

-- | The last frame is not over in case of a strike or spare
-- It is composed of 3 to 4 shots depending on the need to account for strike/spare.
isLastFrameOver :: Frame -> Bool
isLastFrameOver f
    | isLastFrameLonger f = length f >= 4
    | otherwise           = length f >= 3
    -- To have a longer frame:
    -- one strike within the frame is enough
    -- one spare is usually enough except if it is at the first position
    where isLastFrameLonger f =
            S.foldrWithIndex isSpecial False (S.fromList f)
            where isSpecial i (h,_) acc = acc || (h == Strike || (i /= 0 && h == Spare))

-- | The game is over at the last frame when the frame is over
isGameOver :: Board -> Bool
isGameOver b =
    if isLastFrame b && isFrameOver b
        then True
        else False

-- | Bogus  if
-- the board contains more than 5 frames
-- the shot is above 15 (all pins)
isBogus :: Int -> Board -> Bool
isBogus a b = a > all_pins || length b > max_frame

-- | Shots occurs within the StateT monad
-- it returns Nothing when the game is over or the update bogus (nnop)
shot :: Int -> StateT Board Maybe Int
shot a = StateT $ updateBoard a

-- | A new shot has two effects:
--      1. update the board, recording the shot and creating new frame if needed
--      2. update the current score of the game
updateBoard :: Int -> Board -> Maybe (Int, Board)
updateBoard _ [] = error "Please initialize with a non empty board"
updateBoard a b@(currentFrame:xs)
    | isGameOver b || isBogus a b    = Nothing
    | otherwise                      = Just (calcScore newBoard, newBoard)
    where newBoard = (updateFrame a currentFrame b) ++ xs

-- | From the current frame, either
--      - push the new shot in the current frame
--   or - create a new frame containing the sole new shot.
-- Return as a list of one or two frame(s).
updateFrame :: Int -> Frame -> Board -> [Frame]
updateFrame a f b
    | isFrameOver b   = [[newShot a f], f]  -- create a new frame with one new shot
    | otherwise       = [(newShot a f) : f] -- push the new shot in the current frame


-- | Calculate the score of the updated board.
-- First we flatten (concat) the board to remove frame information
-- Then we calc each score chronological wise.
-- Indexing allows to look into the future to account for Strikes & Spares
calcScore :: Board -> Int
calcScore b =
    -- reverse to get all shots in chronological order
    -- it makes it easier to reason about what is "after" one indexed shot
    let allShots = (reverse . concat) b
        indexedShots = S.fromList allShots
    in S.foldlWithIndex (calcShot allShots) 0 indexedShots
    where
        calcShot :: [Shot] -> Int -> Int -> (Hit, Int) -> Int
        calcShot xs acc i (h,a)
            | h == Strike = accScore  + (sumShots $ take 3 after) -- Strike earns the score of the 3 next shots
            | h == Spare = accScore + (sumShots $ take 2 after)  -- Spare earns the score of the 2 next shots
            | h == Normal = accScore
            | otherwise = 0
            where
                accScore = acc + a -- sum the new score in the fold accumulator
                after = drop (succ i) xs -- timewise, list of shots recorded after the current index


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

sumShots :: [Shot] -> Int
sumShots = sum . map snd
