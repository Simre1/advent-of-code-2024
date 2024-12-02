module Day1.Main where

import Control.Applicative.Combinators qualified as P
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (for)
import Data.Vector qualified as V
import Shared
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = ([Int], [Int])

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day1/input"
  pure $ unzip $ (\[a, b] -> (read a, read b)) . words <$> lines file

distances :: Input -> [Int]
distances (left, right) = zipWith dist (L.sort left) (L.sort right)
  where
    dist a b = abs $ b - a

similarity :: Input -> [Int]
similarity (left, right) =
  left <&> \x ->
    x * length (filter (== x) right)

solution1 :: Input -> IO ()
solution1 input = print $ sum $ distances input

solution2 :: Input -> IO ()
solution2 input = print $ sum $ similarity input

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
