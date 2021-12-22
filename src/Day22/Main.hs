module Day22.Main where

import Data.Foldable (Foldable (foldl'))
import Data.List.Split (splitOn)
import Linear.V2 (V2 (..))
import Linear.V3 (V3 (..))

type Cuboid = V3 (V2 Int)

parseInput :: String -> [(Cuboid, Bool)]
parseInput = fmap parseLine . lines
  where
    parseLine l =
      let [state', coords] = splitOn " " l
          state = case state' of "on" -> True; _ -> False
          [x, y, z] = splitOn "," coords
       in ((\[a, b] -> V2 (read a) (read b)) . splitOn ".." . drop 2 <$> V3 x y z, state)

volume :: Cuboid -> Int
volume (V3 x y z) = d x * d y * d z
  where
    d (V2 a b) = if b >= a then abs (b - a) + 1 else 0

calculateActiveCuboids :: [(Cuboid, Bool)] -> [Cuboid]
calculateActiveCuboids = foldl' handleInstruction []
  where
    handleInstruction rebootArea (cuboid, state) =
      let addCube = if state then (cuboid :) else id
       in addCube (rebootArea >>= flip sub cuboid)

sub :: Cuboid -> Cuboid -> [Cuboid]
sub (V3 x1 y1 z1) (V3 x2 y2 z2)
  | overlapping x1 x2 && overlapping y1 y2 && overlapping z1 z2 = filter ((> 0) . volume) [left, right, front, back, top, bottom]
  | otherwise = [V3 x1 y1 z1]
  where
    left = V3 (V2 (l x1) (l x2 - 1)) y1 z1
    right = V3 (V2 (h x2 + 1) (h x1)) y1 z1
    front = V3 (V2 (max (l x1) (l x2)) (min (h x1) (h x2))) (V2 (l y1) (l y2 - 1)) z1
    back = V3 (V2 (max (l x1) (l x2)) (min (h x1) (h x2))) (V2 (h y2 + 1) (h y1)) z1
    top = V3 (V2 (max (l x1) (l x2)) (min (h x1) (h x2))) (V2 (max (l y1) (l y2)) (min (h y1) (h y2))) (V2 (h z2 + 1) (h z1))
    bottom = V3 (V2 (max (l x1) (l x2)) (min (h x1) (h x2))) (V2 (max (l y1) (l y2)) (min (h y1) (h y2))) (V2 (l z1) (l z2 - 1))
    l (V2 a _) = a
    h (V2 _ a) = a
    overlapping (V2 a1 b1) (V2 a2 b2) =
      a1 <= a2 && a2 <= b1 || a1 <= b2 && b2 <= b1 || a2 <= a1 && a1 <= b2 || a2 <= b1 && b1 <= b2

solution1 :: IO ()
solution1 = do
  input <- parseInput <$> readFile "inputs/day22/input1"
  let allCuboids = calculateActiveCuboids input
      cuboidsWithoutInitArea = allCuboids >>= flip sub (V3 (V2 (-50) 50) (V2 (-50) 50) (V2 (-50) 50))
  print $ sum (volume <$> allCuboids) - sum (volume <$> cuboidsWithoutInitArea)
  pure ()

solution2 :: IO ()
solution2 = do
  input <- parseInput <$> readFile "inputs/day22/input1"
  print $ sum $ volume <$> calculateActiveCuboids input
  pure ()
