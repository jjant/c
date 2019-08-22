{-# LANGUAGE TypeFamilies #-}

module Conway
  ( mkGrid
  , basicRule
  , main
  , step
  , render
  , beacon
  , glider
  , blinker
  , at
  , Grid
  , Rule
  ) where

import Control.Comonad (Comonad(..))
import Control.Comonad.Representable.Store
  ( Store(..)
  , StoreT(..)
  , experiment
  , store
  )
import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Data.Bool (bool)
import Data.Distributive (Distributive(..))
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose(..), getCompose)
import Data.Functor.Identity (Identity(..))
import Data.Functor.Rep (Representable(..), distributeRep)
import Data.Vector (Vector, (!), generate, ifoldr)
import System.Environment (getArgs)

type Coord = (Int, (Int, Int))

type Grid = Store (Compose Vector (Compose Vector Vector)) Bool

type Rule = Grid -> Bool

instance Distributive Vector where
  distribute = distributeRep

instance Representable Vector where
  type Rep Vector = Int
  index v i = v ! (i `mod` gridSize)
  tabulate = generate gridSize

gridSize :: Int
gridSize = 20

neighbourCoords :: Int -> [Coord]
neighbourCoords r =
  [ (x, (y, z))
  | x <- [-r .. r]
  , y <- [-r .. r]
  , z <- [-r .. r]
  , (x, y, z) /= (0, 0, 0)
  ]

addCoords :: Coord -> Coord -> Coord
addCoords (x, (y, z)) (x', (y', z')) = (x + x', (y + y', z + z'))

basicRule :: Int -> Rule
basicRule radius g =
  numNeighboursAlive == 3 || (alive && numNeighboursAlive == 2)
  where
    alive = extract g
    neighbours = experiment (at (neighbourCoords radius)) g
    numNeighboursAlive = length (filter id neighbours)

step :: Rule -> Grid -> Grid
step = extend

render :: Grid -> String
render (StoreT (Identity (Compose g)) _) =
  foldMap ((++ "\n") . foldMap (bool "." "#")) g

mkGrid :: [Coord] -> Grid
mkGrid xs = store (`elem` xs) (0, (0, 0))

at :: [Coord] -> Coord -> [Coord]
coords `at` origin = map (addCoords origin) coords

glider :: [Coord]
glider = [(1, (0, 0)), (2, (1, 0)), (0, (2, 0)), (1, (2, 0)), (2, (2, 0))]

blinker = [(0, 0), (1, 0), (2, 0)]

beacon = [(0, 0), (1, 0), (0, 1), (3, 2), (2, 3), (3, 3)]

tickTime :: Int
tickTime = 20000

start :: Grid
start = mkGrid $ glider `at` (0, (0, 0))
    -- ++ beacon `at` (15, 5) ++ glider `at` (20, 20)

gameTimeline :: Int -> [Grid]
gameTimeline radius = iterate (step (basicRule radius)) start

renderXYZ :: Grid -> String
renderXYZ (StoreT (Identity (Compose grid')) _) =
  let grid :: Vector (Vector (Vector Bool))
      grid = getCompose <$> grid'
      liveCellsRendered :: String
      liveCellsRendered =
        ifoldr
          (\row rowVec acc1 ->
             acc1 ++
             ifoldr
               (\col colVec acc2 ->
                  acc2 ++
                  ifoldr
                    (\depth cell acc3 ->
                       acc3 ++
                       bool
                         ""
                         ("\n" ++
                          show row ++ " " ++ show col ++ " " ++ show depth)
                         cell)
                    ""
                    colVec)
               ""
               rowVec)
          ""
          grid
      header = show (length (lines $ liveCellsRendered) - 1)
  in header ++ "\n" ++ liveCellsRendered

-- main :: IO ()
-- main =
--   forM_ (iterate (step basicRule) start) $ \grid -> do
--     putStr "\ESC[2J" -- Clear terminal screen
--     putStrLn (render grid)
--     --putStrLn (renderXYZ grid)
--     threadDelay tickTime
main :: IO ()
main = do
  [numSteps, radius] <- (fmap read) <$> getArgs
  traverse_ putStrLn $ renderXYZ <$> take numSteps (gameTimeline radius)
