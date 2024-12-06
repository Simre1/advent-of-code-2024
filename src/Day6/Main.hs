module Day6.Main where

import Control.Applicative
import Control.Applicative.Combinators qualified as P
import Control.Arrow (Arrow (..))
import Control.Monad
import Control.Monad (guard)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for, forM)
import Data.Vector qualified as V
import Debug.Trace (traceShow, traceShowId)
import Linear.V2
import Shared
import Text.Megaparsec qualified as P hiding (many)
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

data Cell = Obstacle | Free deriving (Eq, Show, Ord)

type Input = ((V2 Int, V2 Int), M.Map (V2 Int) Cell)

toDirection :: Char -> V2 Int
toDirection '>' = V2 1 0
toDirection '<' = V2 (-1) 0
toDirection 'v' = V2 0 1
toDirection '^' = V2 0 (-1)

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day6/input"
  let positions = gridParse file
      start = second toDirection $ head $ filter (\(_, c) -> c `elem` ("<>v^" :: String)) positions
      grid =
        M.fromList $
          fmap
            ( second $ \c -> case c of
                '#' -> Obstacle
                _ -> Free
            )
            positions
  pure (start, grid)

followGuard :: M.Map (V2 Int) Cell -> (V2 Int, V2 Int) -> [(V2 Int, V2 Int)]
followGuard grid (startPosition, startDirection) =
  (startPosition, startDirection)
    : L.unfoldr
      ( \(position, direction) ->
          ( (\x -> (x, x))
              <$> simulateGuard grid (position, direction)
          )
      )
      (startPosition, startDirection)

simulateGuard :: M.Map (V2 Int) Cell -> (V2 Int, V2 Int) -> Maybe (V2 Int, V2 Int)
simulateGuard grid (position, direction) = case M.lookup nextPosition grid of
  Nothing -> Nothing
  Just Free -> Just (nextPosition, direction)
  Just Obstacle -> Just (position, turnRight direction)
  where
    nextPosition = position + direction

turnRight :: V2 Int -> V2 Int
turnRight (V2 dx dy) = V2 (-dy) dx

countLoops :: M.Map (V2 Int) Cell -> [(V2 Int, V2 Int)] -> Int
countLoops grid (start : restPath) =
  length $
    filter id $
      nubOrdOn fst restPath
        <&> ( \(position, direction) ->
                let updatedGrid = M.insert position Obstacle grid
                    updatedPath = followGuard updatedGrid start
                 in isLoop updatedPath
            )

isLoop :: [(V2 Int, V2 Int)] -> Bool
isLoop currentlyWalking = go S.empty currentlyWalking
  where
    go visited (x : xs) | S.member x visited = True
    go visited (x : xs) = go (S.insert x visited) xs
    go _ [] = False

solution1 :: Input -> IO ()
solution1 (start, grid) = do
  let path = fmap fst $ followGuard grid start
  print $ S.size (S.fromList path)

solution2 :: Input -> IO ()
solution2 (start, grid) = do
  let path = followGuard grid start
  -- print path
  print $ countLoops grid path

main :: IO ()
main = do
  input <- readInput
  -- print input
  solution1 input
  solution2 input
