module Day3.Main where

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

type Input = [Instruction]

data Instruction = Mul Int Int | Do | Dont

readInput :: IO Input
readInput = do
  file <- T.readFile "inputs/day3/input"
  pure $ either (const []) id $ P.parse parser "program" file
  where
    parser :: Parser [Instruction]
    parser = haystackparse (mul <|> do_ <|> dont)
    haystackparse :: Parser a -> Parser [a]
    haystackparse p =
      P.choice
        [ (:) <$> p <*> (haystackparse p),
          P.satisfy (const True) >> haystackparse p,
          pure []
        ]
    do_ :: Parser Instruction
    do_ = Do <$ P.string "do()"
    dont :: Parser Instruction
    dont = Dont <$ P.string "don't()"
    mul :: Parser Instruction
    mul = P.try $ do
      P.string "mul("
      a <- numberP
      guard $ a < 1000
      P.string ","
      b <- numberP
      guard $ a < 1000
      P.string ")"
      pure $ Mul a b

filterInstructions :: [Instruction] -> [(Int, Int)]
filterInstructions = go True
  where
    go True (Mul a b : is) = (a, b) : go True is
    go False (Mul a b : is) = go False is
    go _ (Do : is) = go True is
    go _ (Dont : is) = go False is
    go _ [] = []

solution1 :: Input -> IO ()
solution1 input = print $ sum $ [x * y | Mul x y <- input]

solution2 :: Input -> IO ()
solution2 input = print $ sum $ [x * y | (x, y) <- filterInstructions input]

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
