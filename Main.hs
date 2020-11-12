{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.IORef

import Control.Concurrent
import Control.Monad
import GHC.Generics
import Data.Aeson
import Data.Function
import Data.Hashable
import Process.Minizinc
import Process.Minizinc.TH
import Graphics.Gloss.Interface.IO.Animate

genModelData "" "models/mzn-gloss.mzn"

data Building = Building { xcoord :: Int , ycoord :: Int }
  deriving (Show, Ord, Eq)

data Tower = Tower { towerPrice :: Int , towerRange :: Int }
  deriving (Show, Ord, Eq)

buildings :: [Building]
buildings =
  [ Building 1 1
  , Building 1 2
  , Building 1 4
  , Building 5 5
  , Building 6 5
  , Building 7 5
  , Building 1 3
  , Building 8 3
  , Building 2 2
  , Building 7 7
  , Building 4 5
  , Building 5 1
  , Building 10 20
  , Building 10 21
  , Building 20 7
  , Building 10 15
  , Building 13 12
  , Building 13 11
  ]

towers :: [Tower]
towers =
  [ Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 10 2
  , Tower 10 2
  , Tower 10 2
  , Tower 15 3
  , Tower 15 3
  , Tower 15 3
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 10 2
  , Tower 10 2
  , Tower 10 2
  , Tower 15 3
  , Tower 15 3
  , Tower 15 3
  ]

input :: Input
input = Input
  [ pos b | b <- buildings ]
  2
  (length buildings)
  (length towers)
  [ towerPrice t | t <- towers ]
  [ towerRange t | t <- towers ]
  where
    pos :: Building -> [Int]
    pos (Building x y) = [x,y]

main :: IO ()
main = do
    ioref <- newIORef Nothing
    _ <- forkIO $ runSolver ioref
    animateIO disp col (frame ioref) control
  where
    runSolver :: IORef (Maybe Output) -> IO ()
    runSolver ioref = do
        let path = "models/mzn-gloss.mzn"
        let mzn = simpleMiniZinc @Input @Output path 100000 Chuffed
              & withArgs ["-a"]
        runMinizincJSON mzn input () (handler ioref)

    handler :: IORef (Maybe Output) -> ResultHandler Output ()
    handler ioref = ResultHandler (handle1 ioref)

    handle1 :: IORef (Maybe Output)
            -> ()
            -> SearchState Output
            -> IO ((), Maybe (ResultHandler Output ()))
    handle1 ioref _ out = do
        writeIORef ioref $ result out
        threadDelay 1000000
        pure ((), Just $ handler ioref)

    disp = InWindow "mzn-gloss" (400,400) (10,10)

    col = white

    frame ioref timeF = do
        current <- readIORef ioref
        print current
        let towerPictures = case current of
                              Nothing -> []
                              Just output ->
                                  zipWith3 renderTower towers (active output) (towerPosition output)
        pure $ Pictures
             [ Translate (-30) (30)
             $ renderCost (fmap total_cost current)
             , Scale 10 10
             $ Translate (-10) (-10)
             $ Pictures $ [ renderBuilding b | b <- buildings ]
                        <> towerPictures
             ]

    control ctrl = controllerSetRedraw ctrl

renderCost :: Maybe Int -> Picture
renderCost val = Color black
  $ Scale (0.1) (0.1)
  $ Text txt
  where
    txt = case val of
            Nothing -> "no solution found"
            Just v -> "cost: " <> show v

renderBuilding :: Building -> Picture
renderBuilding (Building x y) = Color blue
  $ Translate (fromIntegral x) (fromIntegral y)
  $ Translate (-0.5) (-0.5)
  $ Polygon [(0,0),(1,0),(1,1),(0,1)]

renderTower :: Tower -> Bool -> [Int] -> Picture
renderTower (Tower _ r) True (x:y:[]) = Color red
  $ Translate (fromIntegral x) (fromIntegral y)
  $ Circle (fromIntegral r)
renderTower _ _ _ = Blank
