module Main 

where

import Pipes
import qualified Pipes.Prelude as P
import Afgame(handleShot)

main :: IO ()
main = runEffect $ P.readLn >-> handleShot >-> P.print