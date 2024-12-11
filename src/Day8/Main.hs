module Day8.Main where

import Control.Applicative
import Control.Applicative.Combinators qualified as P
import Control.Monad
import Control.Monad (guard)
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
import Linear.V2
import Linear.Vector ((*^))
import Shared
import Text.Megaparsec qualified as P hiding (many)
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = M.Map (V2 Int) Char

readInput :: IO Input
readInput = do
  grid <- parseFileMega "inputs/day8/input" (matrix2DVP id)
  pure $ M.fromList grid

findInferenceSpots :: V2 Int -> V2 Int -> [V2 Int]
findInferenceSpots a1@(V2 x1 y1) a2@(V2 x2 y2) = [a1 - diff, a2 + diff]
  where
    diff = a2 - a1

inBounds :: (V2 Int, V2 Int) -> V2 Int -> Bool
inBounds (V2 minX minY, V2 maxX maxY) (V2 x y) = minX <= x && x <= maxX && minY <= y && y <= maxY

findHarmonicInferenceSpots :: (V2 Int, V2 Int) -> V2 Int -> V2 Int -> [V2 Int]
findHarmonicInferenceSpots bounds a1@(V2 x1 y1) a2@(V2 x2 y2) =
  takeWhile (inBounds bounds) [a2 + i *^ diff | i <- [0 ..]]
    ++ takeWhile (inBounds bounds) [a1 - i *^ diff | i <- [0 ..]]
  where
    diff = a2 - a1

invertMap :: M.Map (V2 Int) Char -> M.Map Char (S.Set (V2 Int))
invertMap originalMap =
  M.fromListWith (<>) [(v, S.singleton k) | (k, v) <- M.toList originalMap, v /= '.']

accumulateInferenceSpots :: (V2 Int -> V2 Int -> [V2 Int]) -> M.Map (V2 Int) Char -> S.Set (V2 Int)
accumulateInferenceSpots f grid =
  foldMap
    ( \positions ->
        S.fromList $ concat [f a b | a <- S.toList positions, b <- S.toList positions, a /= b]
    )
    antennaPositions
  where
    antennaPositions = invertMap grid

computeBounds :: Input -> (V2 Int, V2 Int)
computeBounds input = (V2 0 0, maximum (M.keys input))

solution1 :: Input -> IO ()
solution1 input = do
  let spots = accumulateInferenceSpots findInferenceSpots input
      bounds = computeBounds input
  print $ S.size $ S.filter (inBounds bounds) spots

solution2 :: Input -> IO ()
solution2 input = do
  let spots = accumulateInferenceSpots (findHarmonicInferenceSpots bounds) input
      bounds = computeBounds input
  print $ S.size spots

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
