module Day15.Main where

import Algorithm.Search
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S

parseInput :: String -> M.Map (Int, Int) Int
parseInput str =
  M.fromList $
    let numberedLines = zip [0 ..] (lines str)
        indexRow (l, r) = zipWith (\c v -> ((c, l), read [v])) [0 ..] r
     in mconcat $ indexRow <$> numberedLines

findPath :: M.Map (Int, Int) Int -> (Int, Int) -> (Int, Int) -> Maybe (Int, [(Int, Int)])
findPath map start end@(endX, endY) = aStar neighbors cost estimate isGoal start
  where
    neighbors :: (Int, Int) -> [(Int, Int)]
    neighbors (x, y) = filter (`M.member` map) [(x + dx, y + dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]
    cost :: (Int, Int) -> (Int, Int) -> Int
    cost _ next = map M.! next
    estimate :: (Int, Int) -> Int
    estimate (x, y) = abs (endX - x) + abs (endY - y)
    isGoal :: (Int, Int) -> Bool
    isGoal = (end ==)

expandTile :: M.Map (Int, Int) Int -> M.Map (Int, Int) Int
expandTile tile = foldl' foldTile M.empty tileCopies
  where
    (tileSizeX, tileSizeY) = (\(a, b) -> (a + 1, b + 1)) $ maximum (M.keys tile)
    tileCopies = [(tileX, tileY) | tileX <- [0 .. 4], tileY <- [0 .. 4]]
    tileElements = M.toList tile
    loopAround n
      | n > 9 = n - 9
      | otherwise = n
    foldTile map (tileX, tileY) =
      foldl'
        (\m ((x, y), riskLevel) -> M.insert (x + tileX * tileSizeX, y + tileY * tileSizeY) (loopAround (riskLevel + tileX + tileY)) m)
        map
        tileElements

solution1 :: IO ()
solution1 = do
  map <- parseInput <$> readFile "inputs/day15/input1"
  let goal = maximum $ M.keys map
  print $ fst <$> findPath map (0, 0) goal

solution2 :: IO ()
solution2 = do
  tile <- parseInput <$> readFile "inputs/day15/input1"
  let map = expandTile tile
      goal = maximum $ M.keys map
  print $ fst <$> findPath map (0, 0) goal
