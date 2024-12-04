module Day4.Main where

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
import Shared
import Text.Megaparsec qualified as P hiding (many)
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Input = String

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day4/input"
  pure file

extractDiagonals :: [[a]] -> [[a]]
extractDiagonals rows =
  rightDiagonals rows
    ++ drop 1 (rightDiagonals (L.transpose $ rows))
    ++ rightDiagonals reversedRows
    ++ drop 1 (rightDiagonals (L.transpose $ reversedRows))
  where
    reversedRows = fmap reverse rows
    rightDiagonals :: [[a]] -> [[a]]
    rightDiagonals xs =
      let shifted = zipWith drop [0 ..] xs
       in L.transpose shifted

countXMAS :: String -> Int
countXMAS ('X' : 'M' : 'A' : 'S' : xs) = 1 + countXMAS xs
countXMAS (x : xs) = countXMAS xs
countXMAS [] = 0

filterMAS :: [(V2 Int, Char)] -> [V2 Int]
filterMAS ((_, 'M') : (p, 'A') : (_, 'S') : xs) = p : filterMAS xs
filterMAS (x : xs) = filterMAS xs
filterMAS [] = []

allLines :: ([String], [String], [String]) -> [String]
allLines (rows, cols, diagonals) = L.concat $ ($) <$> [id, fmap reverse] <*> [rows, cols, diagonals]

solution1 :: Input -> IO ()
solution1 input = do
  let rows = lines input
      cols = L.transpose rows
      diagonals = extractDiagonals rows
  print $ sum $ countXMAS <$> allLines (rows, cols, diagonals)

solution2 :: Input -> IO ()
solution2 file = do
  let rows = lines file
      annotated = annotatePosition rows
      diagonals = extractDiagonals annotated
      masPositions = concat $ filterMAS <$> (diagonals ++ fmap reverse diagonals)
      masCount = M.filter (> 1) $ M.unionsWith (+) $ zipWith M.singleton masPositions (repeat 1)
  print $ M.size masCount

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
