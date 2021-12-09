{-# LANGUAGE TupleSections #-}

module Day9.Main where

import qualified Data.Massiv.Array as M
import qualified Data.Set as S
import Debug.Trace
import Data.List (sortBy)

parseInput :: String -> M.Array M.U M.Ix2 Int
parseInput = M.fromLists' M.Seq . fmap (fmap (read . (: []))) . lines

lowPoints :: M.Stencil M.Ix2 Int Int
lowPoints = M.makeStencil (M.Sz2 3 3) (M.Ix2 1 1) $ \get ->
  let center = get (M.Ix2 0 0)
      surroundings = get <$> [M.Ix2 1 0, M.Ix2 (-1) 0, M.Ix2 0 1, M.Ix2 0 (-1)]
   in if and $ (center <) <$> surroundings
        then center + 1
        else 0

basins :: M.Stencil M.Ix2 (Int, S.Set M.Ix2) (Int, S.Set M.Ix2)
basins = M.makeStencil (M.Sz2 3 3) (M.Ix2 1 1) $ \get ->
  let (center, centerSet) = get (M.Ix2 0 0)
      neighbors' = snd <$> (get <$> neighbors)
   in if center == 0
         then (center, mconcat $ centerSet:neighbors')
         else (center, centerSet)
  where
    neighbors = [M.Ix2 1 0, M.Ix2 (-1) 0, M.Ix2 0 1, M.Ix2 0 (-1)]

solution1 :: IO ()
solution1 = do
  array <- parseInput <$> readFile "inputs/day9/input1"
  let riskyLocations = M.sum $ M.computeS @M.U $ M.mapStencil (M.Fill maxBound) lowPoints array
  print riskyLocations

solution2 :: IO ()
solution2 = do
  array <- M.computeS @M.B . M.imap (\i x -> if x == 9 then (1 :: Int, S.empty) else (0, S.singleton i)) . parseInput <$> readFile "inputs/day9/input1"
  let f = M.computeS @M.B . M.mapStencil (M.Fill (1, S.empty)) basins
  let basinsAfterIteration = iterate f array
  print $ product $ take 3 $ sortBy (flip compare) $ fmap S.size $ S.toList $ foldMap (S.singleton . snd) $ basinsAfterIteration !! 25 -- must be higher than the radius of the largest group. 25 should be enough for most AOC inputs
