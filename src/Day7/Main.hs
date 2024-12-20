module Day7.Main where

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
import Shared
import Text.Megaparsec qualified as P hiding (many)
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P
import Advent

type Input = [(Int, [Int])]

readInput :: IO Input
readInput = parseFileMega "inputs/day7/input" $
  many $ do
    result <- numberP
    lexeme $ P.char ':'
    numbers <- many1 $ lexeme numberP
    P.newline
    pure (result, numbers)

solve :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
solve functions result (n : ns) = any (== result) $ foldl' solve2 [n] ns
  where
    solve2 :: [Int] -> Int -> [Int]
    solve2 bs a = do
      b <- bs
      f <- functions
      pure $ f b a

merge :: Int -> Int -> Int
merge a b = read $ show a ++ show b

solution1 :: Input -> IO ()
solution1 input = print $ sum $ fmap fst $ filter (uncurry (solve [(*), (+)])) input

solution2 :: Input -> IO ()
solution2 input = print $ sum $ fmap fst $ filter (uncurry (solve [(*), (+), merge])) input

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
