module Afgame.Pipes

where

import Afgame
import Afgame.Internal
import Pipes
import qualified Pipes.Prelude as P

prompt :: Producer' Int IO ()
prompt =
    liftIO (putStrLn "Let's start Booling ! Enter your shots.\n(enter 'q' to quit)")
    >> P.stdinLn
    >-> P.takeWhile (/= "q")
    >-> P.read

score' :: Board -> Pipe Int (Int, Board) IO ()
score' = go
    where
        go b = do
            a <- await
            case score a b of
                Right (a', b') -> do
                    yield (a', b')
                    if isGameOver b'
                        then liftIO $ putStrLn "Game over. Bye. See you next time"
                        else go b'
                Left msg -> do
                    liftIO $ putStrLn msg
                    go b

main :: IO ()
main = do
    runEffect $ prompt >-> score' emptyBoard >-> P.print
