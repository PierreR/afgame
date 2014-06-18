module Afgame.Internal (
      allPins
    , Board
    , Frame
    , Hit(..)
    , isFrameOver
    , isLastFrame
    , isLastFrameOver
    , isGameOver
    , Shot
    , sumShots)
where

import Control.Arrow ((>>>))
import qualified Data.Sequence as Seq

-- | A board composed of frames hold the whole game state.
type Board = [Frame]

-- | Shots happens within Frames
-- A frame is a list of shots.
type Frame = [Shot]

-- | There are 15 bowling pins to knock down
allPins :: Int
allPins = 15

-- | the game breaks down into 5 frames
maxFrame :: Int
maxFrame = 5

-- | A shot is defined by a Hit together with the number of pins knocked down.
type Shot =  (Hit, Int)

-- | A shot can be a Strike, a Spare or just Normal.
data Hit = Strike | Spare | Normal deriving (Eq,Show)


--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | The forth frame is always the last frame.
-- PS: '(>>>)' is defined as 'flip (.)' for '(->)'
isLastFrame :: Board -> Bool
isLastFrame = length  >>> (>= maxFrame)

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

-- | A frame is composed of maximum 3 shots and is over whenever all pins are down.
-- The rule is different for the last frame.
isFrameOver :: Board -> Bool
isFrameOver b
    | isLastFrame b = isLastFrameOver f
    | otherwise     = sumShots f >= allPins || length f >= 3
    where f = head b -- take the current frame

-- | The game is over at the last frame when the frame is over
isGameOver :: Board -> Bool
isGameOver b = isLastFrame b && isFrameOver b

sumShots :: Frame -> Int
sumShots = sum . map snd

