module Afgame.Main

where

import Afgame
import Text.Printf

main :: IO ()
main = do
    putStrLn "Let's start Booling !"
    loop emptyBoard
    where
        loop b = do
            putStrLn "Enter your shots.\n(enter 'q' to quit)"
            str <- getLine
            -- TODO Improve input parsing, allow termination with "q"
            case score (read str) b of
                Right (a, b') -> do
                    printf "Score is %s.\nBoard : %s\n" (show a) (show b')
                    if isGameOver b'
                        then putStrLn "Game is over"
                        else loop b'
                Left msg -> do
                    putStrLn msg
                    loop b