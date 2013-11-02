module Main 

where

import Pipes
import qualified Pipes.Prelude as P
import Afgame(hShot)

main :: IO ()
main = do
	board <- runEffect $ P.readLn >-> hShot >-> P.print
	print board