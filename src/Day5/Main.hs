module Day5.Main where

import Control.Monad (forM, forM_)
import Control.Monad.ST (ST, stToIO)
import Data.Foldable (foldl')
import Data.List.Split (splitOn)
import Data.Massiv.Array (Comp (Seq), Ix2 (Ix2), MArray, Sz (Sz2), Sz2, U, write)
import Data.Massiv.Array.Mutable (MArray, modify_, newMArray, write)
import Data.Massiv.Array.Unsafe (unsafeFreeze)
import Data.Massiv.Core.Operations
import Debug.Trace (traceShow, traceShowId)

data Line = Line Ix2 Ix2 deriving (Show)

type MutArray s = MArray s U Ix2 Int

parseInput :: String -> [Line]
parseInput str =
  let ls = lines str
   in flip fmap ls $ \line ->
        let [start, end] = splitOn "->" line
            ([sx, sy], [ex, ey]) = (splitOn "," start, splitOn "," end)
         in Line (Ix2 (read sx) (read sy)) (Ix2 (read ex) (read ey))

createArray :: ST s (MutArray s)
createArray = newMArray (Sz2 1000 1000) 0

addLineVertical :: Line -> MutArray s -> ST s ()
addLineVertical (Line (Ix2 x1 y1) (Ix2 x2 y2)) array
  | x1 /= x2 = pure ()
  | otherwise = forM_ yIndices $ \yIndex -> modify_ array (pure . succ) (Ix2 x1 yIndex)
  where
    yIndices
      | y1 > y2 = [y2 .. y1]
      | otherwise = [y1 .. y2]

addLineHorizontal :: Line -> MutArray s -> ST s ()
addLineHorizontal (Line (Ix2 x1 y1) (Ix2 x2 y2)) array
  | y1 /= y2 = pure ()
  | otherwise = forM_ xIndices $ \xIndex -> modify_ array (pure . succ) (Ix2 xIndex y1)
  where
    xIndices
      | x1 > x2 = [x2 .. x1]
      | otherwise = [x1 .. x2]

addLineDiagonal :: Line -> MutArray s -> ST s ()
addLineDiagonal l@(Line (Ix2 x1 y1) (Ix2 x2 y2)) array
  | y1 == y2 || x1 == x2 = pure ()
  | otherwise = forM_ dIndices $ modify_ array (pure . succ)
  where
    yIndices
      | y1 > y2 = [y1, y1 -1 .. y2]
      | otherwise = [y1 .. y2]
    xIndices
      | x1 > x2 = [x1, x1 -1 .. x2]
      | otherwise = [x1 .. x2]
    dIndices = zipWith Ix2 xIndices yIndices

calculate :: Foldable t => t a -> (MutArray s -> a -> ST s ()) -> ST s Int
calculate input lines = do
  marray <- createArray
  forM_ input $ lines marray
  arr <- unsafeFreeze Seq marray
  pure $ foldArray (\b a -> (if a > 1 then 1 else 0) + b) 0 arr

solution1 :: IO ()
solution1 = do
  input <- parseInput <$> readFile "inputs/day5/input1"
  solution <- stToIO $
    calculate input $
      \marray line -> addLineHorizontal line marray >> addLineVertical line marray
  print solution

solution2 :: IO ()
solution2 = do
  input <- parseInput <$> readFile "inputs/day5/input1"
  solution <- stToIO $
    calculate input $
      \marray line -> addLineHorizontal line marray >> addLineVertical line marray >> addLineDiagonal line marray
  print solution
