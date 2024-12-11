module Day11.Main where

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
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for, forM)
import Data.Vector qualified as V
import Debug.Trace (traceShow)
import Shared
import Text.Megaparsec qualified as P hiding (many)
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = [Int]

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day11/input"
  pure $ fmap read $ words file

makeStoneMap :: Input -> M.Map Int Int
makeStoneMap input = foldl1 (M.unionWith (+)) $ fmap (\stone -> M.singleton stone 1) input

evolveStones :: M.Map Int Int -> M.Map Int Int
evolveStones = M.foldrWithKey step M.empty
  where
    step stoneNumber amount otherStones =
      let newStones = makeStoneMap $ evolveStone stoneNumber
       in M.unionWith (+) ((amount *) <$> newStones) otherStones

evolveStone :: Int -> [Int]
evolveStone x
  | x == 0 = [1]
  | even digitsAmount =
      let half = digitsAmount `quot` 2
       in read <$> [take half digits, drop half digits]
  | otherwise = [x * 2024]
  where
    digits = show x
    digitsAmount = length digits

solution1 :: Input -> IO ()
solution1 input =
  print $ sum $ applyNTimes 25 evolveStones $ makeStoneMap input

solution2 :: Input -> IO ()
solution2 input =
  print $ sum $ applyNTimes 75 evolveStones $ makeStoneMap input

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
