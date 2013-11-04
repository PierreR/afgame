{-# LANGUAGE RankNTypes #-}
module Main 

where

import Pipes
import qualified Pipes.Prelude as P
import Afgame

--testme :: Consumer Int IO String
testme =  (lift $ fmap read getLine) >~ score >~ scorePrinter >~ P.stdoutLn

prompt :: Producer' Int IO ()
prompt = (liftIO $ putStrLn "Let's start Booling ! Enter your shots.\n(enter 'q' to quit)") >> P.stdinLn
     >-> P.takeWhile (/= "q") >-> P.read

main :: IO ()
main =  runEffect $ prompt >-> (score >~ scorePrinter >~ P.stdoutLn)