module Main 

where

import Pipes
import qualified Pipes.Prelude as P
import Afgame

main :: IO ()
main =  runEffect $ score prompt >-> P.print