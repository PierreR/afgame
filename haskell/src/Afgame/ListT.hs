{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Afgame.ListT

where

import           Control.Monad.State.Strict
import           Data.Monoid
import           List.Transformer
import qualified System.IO
import           Text.Printf                (printf)
import           Text.Read                  (readMaybe)

import           Afgame
import           Afgame.Internal

prompt :: (MonadIO m) => ListT m Int
prompt = ListT go
  where
    go = do
      -- Ctrl-D tells the terminal that it should register an EOF on standard input, which bash interprets as a desire to exit.
      eof <- liftIO System.IO.isEOF
      if eof
         then return Nil
         else do
           str <- liftIO getLine
           if (str == "q")
             then return Nil
             else case (readMaybe str) of
                    Just a -> return (Cons a prompt)
                    _      -> liftIO (putStrLn "Ignoring input. Please enter a number") >> go

displayBoardInfo :: Board -> String
displayBoardInfo b =
  if isFrameOver b
    then "Frame is over. You will be playing frame " <> show (length b + 1)
    else "You are playing frame " <>  show (length b)

consumer :: (MonadState Board m, MonadIO m) => ListT m Int -> m ()
consumer = go
  where
    go xs = do
      x <- next xs
      case x of
        Nil         -> liftIO $ printf "Thanks for playing. See you next time. Bye\n"
        Cons x' xs' -> do
          b <- get
          case score x' b of
            Right (Current (a', b')) -> do
                liftIO $ printf "Your current score is %u. %s\n"  a' (displayBoardInfo b')
                put b'
                go xs'
            Right (Done s) -> liftIO $ printf "Game over. Your final score is %u. See you next time. Bye\n" s
            Left msg -> do
                liftIO $ putStrLn msg
                go xs'

main :: IO ()
main = do
  putStrLn "Let's start Booling ! Enter your shots.\n(enter 'q' or the usual 'ctrl-d' to exit)"
  evalStateT (consumer prompt) emptyBoard
