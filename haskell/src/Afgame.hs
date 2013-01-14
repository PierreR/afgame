------------------------------------------------------------------------------
-- | This module calculates the score of the african game.
-- The definition of the game is given by the pdf at the root of the project.
-- It uses a state monad that hold all 'current scores' and the
-- status of the game (called the board).
-- One important note :
-- According to the definition of the game, a strike or spare earns the point of 
-- the next three or two shots. The current implementation just adds the "after" shots
-- (the pins knocked down), not the real score obtained by these shots

module Afgame (shot, Hit(..))
where

import Control.Monad.State

import qualified Data.Sequence as S

-------------------------------------------------------------------------------
-- 
--In case of a strike or spare, the score of one shot will depend on the next
-- shots. Consequently there is not point to calculate the score while we update
-- the board and this implementation separate the two concerns.
------------------------------------------------------------------------------ 


-- A shot can be  a Strike (all pins knocked down in one shot), 
--                a Spare (all pins knocked down within two shots
--                or just Normal
data Hit = Strike | Spare | Normal deriving (Eq,Show)

-- A shot is defined by the hit together with the number of pins knocked down
type Shot =  (Hit, Int)

-- A Frame is a list of shots
type Frame = [Shot]

-- A board composed of frames can handle the whole game state
type Board = [Frame]

-- | The forth frame is always the last frame of a board.
isLastFrame :: Board -> Bool
isLastFrame b = length b >= 5

-- | A frame is composed of maximum 3 shots and if over if all pins are down 
--   However the last frame is allowed to go up to 4 shots to account for a strike or a spare
isFrameOver :: Frame -> Board -> Bool
isFrameOver f b
    | isLastFrame b = length f >= 4
    | otherwise     = sumShots f >= 15 || length f >= 3

-- | Shots occurs with the State monad.
shot :: Int -> State Board Int
shot a = state $ updateBoard a

-- | A new shot has two effects:
--      1. update the board, recording the shot and creating new frame if needed
--      2. update the current score of the game
updateBoard :: Int -> Board -> (Int, Board)
updateBoard _ [] = error "Please initialize with a non empty board"
updateBoard a board@(currentFrame:xs) =
    (calcScore newBoard, newBoard)
        where newBoard = (updateFrame a currentFrame board) ++ xs

-- | From the current frame, either
--      push the new shot in the current frame
--   or create a new frame containing the sole new shot.
--   Return as a list of one or two frame(s)
updateFrame :: Int -> Frame -> Board -> [Frame]
updateFrame a f b
    | isFrameOver f b = [[newShot a f], f]  -- create a new frame with one new shot
    | otherwise       = [(newShot a f) : f] -- push the new shot in the current frame

-- | Within a frame, from the number of pins knockned down,
--      decide what hit qualified the score
--      In other words, construct the Shot datatype.
newShot :: Int -> Frame -> Shot
newShot a  f
    | a == 15                   -- Strike !
                                = (Strike, 15)
    | not (null f)              -- check the list is not empty 
      && a + previousShot == 15 -- Spare if we score exactly 15 with the previous shot
      && previousHit /= Spare   -- check previous was not a spare (it can happen in the last frame)
                                = (Spare, a)
    | otherwise
                                = (Normal, a)
    where (previousHit, previousShot) = head f

-- | Calculate the score of the updated board.
--   First we flatten (concat) the board to remove frames information
--   Then we calc each score chronological wise:
--     indexing allows to look into the future to account for Strikes & Spares
calcScore :: Board -> Int
calcScore b =
    -- reverse to get all shots in chronological order
    -- it makes it easier to reason about what is "after" one shot
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

-- utilities

sumShots :: [Shot] -> Int
sumShots xs = sum $ map snd xs
