module Afgame.Pipes

where

import Afgame
import Afgame.Internal
import Pipes
import Text.Printf (printf)
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
                Right (Current (a', b')) -> do
                    yield (a', b')
                    go b'
                Right (Done s) -> liftIO $ printf "Game over. Your final score is %u. See you next time. Bye\n" s
                Left msg -> do
                    liftIO $ putStrLn msg
                    go b

main :: IO ()
main = do
    runEffect $ prompt >-> score' emptyBoard >-> P.print
