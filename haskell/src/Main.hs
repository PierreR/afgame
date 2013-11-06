{-# LANGUAGE RankNTypes #-}
module Main 

where

import Pipes
import Pipes.Lift
import qualified Control.Monad.State.Strict as S
import qualified Pipes.Prelude as P
import Afgame

--testme :: Consumer Int IO String
--testme =  (lift $ fmap read getLine) >~ score >~ scorePrinter >~ P.stdoutLn



counter :: Monad m => Pipe Char Int (S.StateT Int m) ()
counter = go
  where
    go = do
        x <- await
        lift $ S.modify (+1)
        r <- lift S.get
        yield r
        go

localCounter :: Monad m => Pipe Char Int m ()
localCounter = evalStateP 0 counter

main :: IO ()
main =  --runEffect $ each "hi there" >-> localCounter >-> P.printt
    runEffect $ score prompt >-> P.print