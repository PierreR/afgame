module Afgame.Pipes

where

import Afgame --hiding (score)
import qualified Control.Monad.State.Strict as S
import Control.Monad.Trans.Either
import Pipes
import qualified Pipes.Prelude as P
import qualified Pipes.Lift as PL
--import qualified Pipes.Parse as PP

prompt :: Producer' Int IO ()
prompt =
    liftIO (putStrLn "Let's start Booling ! Enter your shots.\n(enter 'q' to quit)")
    >> P.stdinLn
    >-> P.takeWhile (/= "q")
    >-> P.read

-- TODO TRY REVERT THIS
parseShot :: MonadIO m => Producer Int IO () -> Producer (Int, Board) (S.StateT Board (EitherT String m)) ()
parseShot = go
    where
        go p = do
            b <- lift S.get
            x <- liftIO $ next p
            case x of
                Right (a, p') -> do
                    case score a b of
                        Right (a', b') -> do
                            yield (a', b')
                            if isGameOver b'
                                then liftIO $ putStrLn "Game Over"
                                else go p'
                        Left msg -> liftIO $ putStrLn msg
                Left () -> return ()

-- | Feed with Shot, `score` produce the tuple (Score, Board)
--score' :: MonadIO m => Producer Int IO () -> Producer (Int, Board) (EitherT String m) ()
--score' p = PL.evalStateP emptyBoard (parseShot p)

--main :: IO ()
--main = do
--    runEffect $ score' prompt >-> P.print