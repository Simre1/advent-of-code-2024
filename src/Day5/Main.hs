module Day5.Main where

import Control.Applicative
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
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

type Rules = M.Map Int [Int]

type Input = ([(Int, Int)], [[Int]])

readInput :: IO Input
readInput = parseFileMega "inputs/day5/input" parser
  where
    parser :: Parser Input
    parser = (,) <$> many rule <*> many pages
    rule :: Parser (Int, Int)
    rule = lexemeFull $ P.try $ do
      n1 <- numberP
      P.char '|'
      n2 <- numberP
      pure (n1, n2)
    pages :: Parser [Int]
    pages = lexemeFull $ numberP `P.sepBy1` P.char ','

makeRules :: [(Int, Int)] -> Rules
makeRules = foldl' (\m (left, right) -> M.alter (Just . maybe [right] (right :)) left m) M.empty

validateUpdate :: Rules -> [Int] -> Bool
validateUpdate rules = go []
  where
    go prev (current : next) =
      let mustBeAfter = fromMaybe [] $ rules M.!? current
          pageIsValid = all (\x -> x `notElem` prev) mustBeAfter
       in pageIsValid && go (current : prev) next
    go _ [] = True

fixUpdate :: Rules -> [Int] -> [Int]
fixUpdate rules = foldl' insert []
  where
    insert :: [Int] -> Int -> [Int]
    insert correct x = fromJust $ L.find (validateUpdate rules) $ insertEverywhere correct x

insertEverywhere :: [Int] -> Int -> [[Int]]
insertEverywhere [] i = [[i]]
insertEverywhere (x : xs) i = (i : x : xs) : fmap (x :) (insertEverywhere xs i)

solution1 :: Input -> IO ()
solution1 (ruleList, updates) = do
  let rules = makeRules ruleList
      validUpdates = filter (validateUpdate rules) updates
      middlePages = (\l -> l !! (length l `quot` 2)) <$> validUpdates
  print $ sum middlePages

solution2 :: Input -> IO ()
solution2 (ruleList, updates) = do
  let rules = makeRules ruleList
      invalidUpdates = filter (not . validateUpdate rules) updates
      fixedUpdates = fixUpdate rules <$> invalidUpdates
      middlePages = (\l -> l !! (length l `quot` 2)) <$> fixedUpdates
  print $ sum middlePages

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
