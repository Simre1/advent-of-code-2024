module Day10.Main where

import Control.Applicative
import Control.Applicative.Combinators qualified as P
import Control.Monad
import Control.Monad (guard)
import Data.Containers.ListUtils (nubOrdOn)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Massiv.Array qualified as A
import Data.Maybe
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for, forM)
import Data.Vector qualified as V
import Shared
import Text.Megaparsec qualified as P hiding (many)
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = A.Array A.S A.Ix2 Int

readInput :: IO Input
readInput = parseFileMega "inputs/day10/input" (parseStorableMatrix (read . pure))

findTrailHeads :: Input -> [A.Ix2]
findTrailHeads input = S.toList $ A.ifoldMono (\ix e -> if e == 0 then S.singleton ix else S.empty) input

findNext :: Input -> A.Ix2 -> [A.Ix2]
findNext input ix =
  let currentHeight = input A.! ix
      neighbors =
        [ ix + direction
        | direction <- (*) <$> [1, -1] <*> [A.Ix2 1 0, A.Ix2 0 1],
          neighbor <- input A.!? (direction + ix),
          neighbor == currentHeight + 1
        ]
   in neighbors

findPaths :: Input -> A.Ix2 -> [[A.Ix2]]
findPaths input start = go start
  where
    go position = do
      let currentValue = input A.! position
      if currentValue == 9
        then [[position]]
        else do
          next <- findNext input position
          ((position :) <$> go next)

solution1 :: Input -> IO ()
solution1 input = do
  let trailHeads = findTrailHeads input
  print $ sum $ do
    trailHead <- trailHeads
    pure $ length $ nubOrdOn last $ findPaths input trailHead

solution2 :: Input -> IO ()
solution2 input = do
  let trailHeads = findTrailHeads input
  print $ sum $ do
    trailHead <- trailHeads
    pure $ length $ findPaths input trailHead

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
