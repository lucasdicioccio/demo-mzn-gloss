{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.IORef

import Control.Concurrent
import Control.Monad
import GHC.Generics
import Data.List ((\\))
import Data.Aeson
import Data.Function
import Data.Hashable
import Process.Minizinc
import Process.Minizinc.TH
import Graphics.Gloss.Interface.IO.Game

genModelData "" "models/mzn-gloss.mzn"

data Building = Building { xcoord :: Int , ycoord :: Int }
  deriving (Show, Ord, Eq)

data Tower = Tower { towerPrice :: Int , towerRange :: Int }
  deriving (Show, Ord, Eq)

defaultBuildings :: [Building]
defaultBuildings =
  [ 
  ]

towers :: [Tower]
towers =
  [ Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 5 1
  , Tower 10 2
  , Tower 10 2
  , Tower 10 2
  , Tower 10 2
  , Tower 10 2
  , Tower 10 2
  , Tower 15 3
  , Tower 15 3
  , Tower 15 3
  , Tower 15 3
  , Tower 15 3
  , Tower 15 3
  ]

input :: [Building] -> Input
input blds = Input
  [ pos b | b <- blds ]
  1
  (length blds)
  (length towers)
  [ towerPrice t | t <- towers ]
  [ towerRange t | t <- towers ]
  where
    pos :: Building -> [Int]
    pos (Building x y) = [x,y]

data World = World {
    ioref :: IORef (Maybe Output)
  , solverThread :: Maybe ThreadId
  , buildings :: [ Building ]
  }

main :: IO ()
main = do
    ioref <- newIORef Nothing
    let world = World ioref Nothing defaultBuildings
    playIO disp col 1 world render handleInput control
  where
    runSolver :: Input -> IORef (Maybe Output) -> IO ()
    runSolver input ioref = do
        let path = "models/mzn-gloss.mzn"
        let mzn = simpleMiniZinc @Input @Output path 300000 Chuffed
              & withArgs ["-a"]
        runMinizincJSON mzn input () (outputhandler ioref)

    outputhandler :: IORef (Maybe Output) -> ResultHandler Output ()
    outputhandler ioref = ResultHandler (handle1 ioref)

    handle1 :: IORef (Maybe Output)
            -> ()
            -> SearchState Output
            -> IO ((), Maybe (ResultHandler Output ()))
    handle1 ioref _ out = do
        writeIORef ioref $ result out
        threadDelay 300000
        pure ((), Just $ outputhandler ioref)

    disp = InWindow "mzn-gloss" (400,400) (0,0)

    col = white

    render world = do
       current <- readIORef (ioref world)
       print current
       let towerPictures = case current of
                              Nothing -> []
                              Just output ->
                                  zipWith3 renderTower towers (active output) (towerPosition output)
       pure $ Translate (-180) (-180)
             $ Pictures
                 [ Translate (-10) (-10) $ renderCost (fmap total_cost current)
                 , Pictures $ [ renderBuilding b | b <- buildings world ] <> towerPictures
                 ]
    handleInput (EventKey (SpecialKey KeySpace) Down _ _) w = do
       restartSolver w
    handleInput (EventKey (MouseButton LeftButton) Down _ (x,y)) w = do
       reverseBuilding w (reverseCoord x, reverseCoord y) >>= restartSolver
    handleInput _ w = pure w

    reverseCoord v = ceiling $ (v + 180) / 10 - 0.5

    restartSolver w = do
          case solverThread w of
              Nothing -> pure ()
              Just otId -> killThread otId
          writeIORef (ioref w) Nothing
          tId <- forkIO $ runSolver (input $ buildings w) (ioref w)
          pure $ w { solverThread = Just tId }

    reverseBuilding world (x,y) = do
          let b = Building x y
          if b `elem` (buildings world)
          then pure $ world { buildings = buildings world \\ [b]}
          else pure $ world { buildings = b : buildings world }
       

    control dt w = pure w

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
  $ Translate (10 * fromIntegral x) (10 * fromIntegral y)
  $ Scale (10.0) (10.0)
  $ Translate (-0.5) (-0.5)
  $ Polygon [(0,0),(1,0),(1,1),(0,1)]

renderTower :: Tower -> Bool -> [Int] -> Picture
renderTower (Tower _ r) True (x:y:[]) = Color red
  $ Translate (10 * fromIntegral x) (10 * fromIntegral y)
  $ Scale (10.0) (10.0)
  $ Circle (fromIntegral r)
renderTower _ _ _ = Blank
