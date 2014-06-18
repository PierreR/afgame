{-# LANGUAGE ScopedTypeVariables #-}
------------------------------------------------------------------------------
-- | This module calculates the score of the African game.
-- See <https://github.com/PierreR/afgame/README.md> for the specifications.
-- `scores` uses a state monad to hold all 'current scores' (the value) together with
-- the status of the game (called the board).
-- One important note :
-- According to the definition of the game, a strike or spare earns the point of
-- the next three or two shots. This implementation just adds the "after" shots
-- (the pins knocked down), not the real score obtained by these shots.
-- The Internal module contains the model and non-exposed utilities.
module Afgame (
      ScoreBoard(..)
    , score
    , scores
    , emptyBoard
)
where

import Afgame.Internal
import qualified Data.Sequence as Seq

-------------------------------------------------------------------------------
--
-- In case of a strike or spare, the score of one shot will depend on the next
-- shots. Consequently there is not point to calculate the score while we update
-- the board and this implementation separate the two concerns.
------------------------------------------------------------------------------

-- | Strike when all the pins are knocked down
isStrike :: Int -> Bool
isStrike x = x == allPins

-- | Spare when
--     1. the frame is not empty
--     2. all pins are knocked down within 2 shots
--     3. the previous shot is not a spare (only happens in the last frame)
isSpare :: Int -> Frame -> Bool
isSpare x f =
    let (previousHit, previousShot) = head f
    in  not (null f)
        && x + previousShot == allPins
        && previousHit /= Spare

isShotBogus :: Int -> Bool
isShotBogus x = x > allPins || x < 0

emptyBoard :: Board
emptyBoard = [[]]

data ScoreBoard = Done Int | Current (Int, Board) deriving (Show, Eq)

-- | Push the new shot in the current frame or
--   if the current frame is over, create a new frame containing the sole new shot.
updateGame :: Int -> Board -> Either String Board
updateGame _ [] = Left "Board game should be initialized"
updateGame x b@(currentFrame:xs)
    | isShotBogus x = Left "Invalid shot. Try again."
    | isGameOver b  = Left "This game is over !"
    | isFrameOver b = Right ([newShot] : b) -- create a new frame with one new shot
    | otherwise     = do
        let newFrame = newShot : currentFrame
        -- Except for the last frame, the shot is bogus when it adds up for more than 15
        if isLastFrame b || sumShots newFrame <= allPins
            then Right (newFrame :xs)
            else
                Left  "This shot creates an invalid frame. Try again."
    where
        -- | Within a Frame, from the number of pins down, construct a 'Shot'.
        newShot
            | isStrike x   = (Strike, allPins)
            | isSpare x currentFrame  = (Spare, x)
            | otherwise    = (Normal, x)


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
        calcShot xs acc i (h,x)
            | h == Strike = accScore + sumShots (take 3 after) -- Strike earns the score of the 3 next shots
            | h == Spare  = accScore + sumShots (take 2 after) -- Spare earns the score of the 2 next shots
            | h == Normal = accScore
            | otherwise = 0
            where
                accScore = acc + x -- sum the new score in the fold accumulator
                after = drop (succ i) xs -- timewise, list of shots recorded after the current index

-- | Given a shot and a board, either returns an error msg or produces (Score, Board)
score :: Int -> Board -> Either String ScoreBoard
score x b = do
    b' <- updateGame x b
    let s = calcScore b'
    if isGameOver b'
        then return $ Done s
        else return $ Current (s, b')

scores :: [Int] -> Board -> Either String ScoreBoard
scores (x:xs) b = 
   case score x b of
      Right c@(Current (_, b')) ->
          if null xs
          then return c
          else scores xs b'
      Right (Done s) -> Right $ Done s
      Left s -> Left s
scores [] _ = Left "Empty input. Try again"
