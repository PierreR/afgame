module Main 

where

import Pipes
import qualified Pipes.Prelude as P
import Afgame(score)

prompt :: Producer String IO ()
prompt = (liftIO $ putStrLn "Let's start Booling ! Enter your shots.\n(enter 'q' to quit)") >> P.stdinLn

main :: IO ()
main = runEffect $ prompt >-> P.takeWhile (/= "q") >-> P.read >-> score >-> P.stdoutLn