{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Data.IORef

import Control.Concurrent
import Control.Monad
import GHC.Generics
import Data.Aeson
import Data.Function
import Data.Hashable
import Process.Minizinc
import Graphics.Gloss.Interface.IO.Animate

data Input = Input { x :: Int }
  deriving (Generic, ToJSON, Hashable)

data Output = Output { y :: Int }
  deriving (Generic, FromJSON, Show)

main :: IO ()
main = do
    ioref <- newIORef Nothing
    _ <- forkIO $ forever $ runSolver ioref
    animateIO disp col (frame ioref) control
  where
    runSolver :: IORef (Maybe Output) -> IO ()
    runSolver ioref = do
        let path = "models/mzn-gloss.mzn"
        let mzn = simpleMiniZinc @Input @Output path 10000 Gecode
              & withArgs ["-a"]
        runMinizincJSON mzn (Input 100) () (handler ioref)

    handler :: IORef (Maybe Output) -> ResultHandler Output ()
    handler ioref = ResultHandler (handle1 ioref)

    handle1 :: IORef (Maybe Output)
            -> ()
            -> SearchState Output
            -> IO ((), Maybe (ResultHandler Output ()))
    handle1 ioref _ out = do
        writeIORef ioref $ result out
        threadDelay 10000
        pure ((), Just $ handler ioref)

    disp = InWindow "mzn-gloss" (400,400) (10,10)

    col = white

    frame ioref timeF = do
        current <- readIORef ioref
        let val = maybe 0 y (current)
        let v = fromIntegral val
        pure $ Color red
             $ Translate v v
             $ Scale (0.5) (0.5)
             $ ThickCircle 5 2

    control ctrl = controllerSetRedraw ctrl
