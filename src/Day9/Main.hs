module Day9.Main where

import Control.Applicative
import Control.Applicative.Combinators qualified as P
import Control.Monad
import Control.Monad (guard)
import Control.Monad.Loops
import Control.Monad.ST
import Data.Bifunctor (Bifunctor (..))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List qualified as L
import Data.List.Split qualified as LS
import Data.Map qualified as M
import Data.Maybe
import Data.STRef
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Traversable (for, forM)
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Debug.Trace
import Shared
import Text.Megaparsec qualified as P hiding (many)
import Text.Megaparsec.Char qualified as P
import Text.Megaparsec.Char.Lexer qualified as P

newtype Disk = Disk (VS.Vector Int) deriving (Eq, Show)

newtype FreeMap = FreeMap (M.Map Int [Int]) deriving (Eq, Show)

newtype FileMap = FileMap (M.Map Int (Int, Int)) deriving (Eq, Show)

type Input = [Int]

readInput :: IO Input
readInput = do
  file <- readFile "inputs/day9/input"
  let diskDescription = fmap (read . pure) $ filter (\x -> '0' <= x && x <= '9') file
  pure diskDescription

fillDisk :: Input -> Disk
fillDisk = Disk . VS.fromList . go 0
  where
    go id (file : free : otherFiles) = replicate file id ++ replicate free (-1) ++ go (succ id) otherFiles
    go id [file] = replicate file id
    go _ [] = []

simpleCompactDisk :: Disk -> Disk
simpleCompactDisk (Disk vector) = Disk $ runST $ do
  fileData <- VS.thaw vector
  leftIndex <- newSTRef 0
  rightIndex <- newSTRef $ VSM.length fileData - 1
  advanceToEmpty fileData leftIndex
  backToData fileData rightIndex

  whileM_ ((<) <$> readSTRef leftIndex <*> readSTRef rightIndex) $ do
    left <- readSTRef leftIndex
    right <- readSTRef rightIndex
    writeData <- VSM.read fileData right
    VSM.write fileData left writeData
    VSM.write fileData right (-1)

    advanceToEmpty fileData leftIndex
    backToData fileData rightIndex

  VS.freeze fileData

advanceToEmpty fileData ptr = do
  whileM_
    ( do
        x <- readSTRef ptr >>= VSM.readMaybe fileData
        pure $ x >= Just 0
    )
    (modifySTRef ptr succ)

backToData fileData ptr = do
  whileM_
    ( do
        x <- readSTRef ptr >>= VSM.read fileData
        pure $ x < 0
    )
    (modifySTRef ptr pred)

wholeFileCompactDisk :: Disk -> Disk
wholeFileCompactDisk (Disk vector) = Disk $ runST $ do
  fileData <- VS.thaw vector
  leftIndex <- newSTRef 0
  rightIndex <- newSTRef $ VSM.length fileData - 1

  whileM_ (readSTRef rightIndex <&> (> 0)) $ do
    backToData fileData rightIndex

    fileSize <- countBlockSize (-) fileData rightIndex

    freeSizePtr <- newSTRef 0
    writeSTRef leftIndex 0
    advanceToEmpty fileData leftIndex
    countBlockSize (+) fileData leftIndex >>= writeSTRef freeSizePtr

    whileM_
      ( do
          freeSize <- readSTRef freeSizePtr
          left <- readSTRef leftIndex
          right <- readSTRef rightIndex
          pure $ freeSize < fileSize && right > left
      )
      ( do
          freeSize <- readSTRef freeSizePtr
          modifySTRef leftIndex (+ freeSize)
          advanceToEmpty fileData leftIndex
          countBlockSize (+) fileData leftIndex >>= writeSTRef freeSizePtr
      )

    left <- readSTRef leftIndex
    right <- readSTRef rightIndex

    when (left < right) $ do
      forM_ [0 .. fileSize - 1] $ \i -> do
        writeData <- VSM.read fileData (right - i)
        VSM.write fileData (left + i) writeData
        VSM.write fileData (right - i) (-1)

    modifySTRef rightIndex (\x -> x - fileSize)

  VS.freeze fileData

countBlockSize direction fileData ptr = do
  i <- readSTRef ptr
  maybeX <- VSM.readMaybe fileData i

  case maybeX of
    Nothing -> pure 0
    Just x -> do
      size <- newSTRef 0

      whileM_
        ( do
            s <- readSTRef size
            v <- VSM.readMaybe fileData (i `direction` s)
            pure $ v == Just x
        )
        (modifySTRef size succ)

      readSTRef size

checksum :: Disk -> Int
checksum (Disk fileData) = VS.ifoldl' (\s i x -> max 0 x * i + s) 0 fileData

fillMaps :: Input -> (FreeMap, FileMap)
fillMaps = bimap FreeMap FileMap . go 0 0
  where
    go id diskIndex (file : free : otherFiles) =
      let (freeMap, fileMap) = go (succ id) (diskIndex + file + free) otherFiles
       in (M.alter (pure . maybe [diskIndex + file] (++ [diskIndex + file])) free freeMap, M.insert id (file, diskIndex) fileMap)
    go id diskIndex [file] = (M.empty, M.insert id (file, diskIndex) M.empty)
    go _ _ [] = (M.empty, M.empty)

-- compactFileMap :: (FreeMap, FileMap) -> FileMap
-- compactFileMap (FreeMap initialFreeMap, FileMap initialFileMap) = FileMap $ snd $ M.mapAccum compactFile initialFreeMap initialFileMap
--   where
--     compactFile :: M.Map Int [Int] -> (Int, Int) -> (M.Map Int [Int], (Int, Int))
--     compactFile freeMap (fileSize, diskPtr) = case M.lookup fileSize freeMap of
--       Nothing -> (freeMap, (fileSize, diskPtr))
--       Just [] -> (freeMap, (fileSize, diskPtr))
--       Just (f:fs) ->

-- advanceToEmpty fileData ptr = do
--   whileM_
--     ( do
--         x <- readSTRef ptr >>= VSM.read fileData
--         pure $ x < 0
--     )
--     (modifySTRef ptr succ)

solution1 :: Input -> IO ()
solution1 input = print $ checksum $ simpleCompactDisk $ fillDisk input

solution2 :: Input -> IO ()
solution2 input = print $ checksum $ wholeFileCompactDisk $ fillDisk input

main :: IO ()
main = do
  input <- readInput
  solution1 input
  solution2 input
