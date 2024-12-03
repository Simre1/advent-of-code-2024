module Day2.Main where

import Control.Applicative.Combinators qualified as P
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for, forM)
import Data.Vector qualified as V
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = [Report]

type Report = [Int]

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day2/input"
  pure $ fmap read . words <$> lines file

isSafe :: Report -> Bool
isSafe report =
  let distances = combine dist report
      signs = signum <$> distances
   in all (head signs ==) signs && all (\d -> 1 <= abs d && abs d <= 3) distances
  where
    combine f (a : b : xs) = f a b : combine f (b : xs)
    combine _ _ = []
    dist a b = b - a

dampenReport :: Report -> [Report]
dampenReport report = report : removeOne report
  where
    removeOne (x : xs) = xs : ((x :) <$> removeOne xs)
    removeOne [] = []

solution1 :: Input -> IO ()
solution1 input = do
  print $ length $ filter id $ isSafe <$> input

solution2 :: Input -> IO ()
solution2 input = do
  print $ length $ filter id $ any isSafe . dampenReport <$> input

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
