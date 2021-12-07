module Day6.Main where

import Data.List (group, sort)
import Data.List.Split (splitOn)
import qualified Data.Vector as V

type Population = V.Vector Int

parseInitialPopulation :: String -> Population
parseInitialPopulation input =
  let fish = read @Int <$> splitOn "," input
   in V.fromList $ pred . length <$> group (sort ([0 .. 8] ++ fish))

simulatePopulation :: Population -> Population
simulatePopulation previousPopulation =
  V.generate populationClasses (\popClass -> sum $ (previousPopulation V.!) <$> from popClass)
  where
    populationClasses = 9
    from 6 = [0, 7]
    from 8 = [0]
    from i = [succ i]

solution1 :: IO ()
solution1 = do
  initialPopulation <- parseInitialPopulation <$> readFile "inputs/day6/input1"
  print $ V.sum $ (!! 80) $ iterate simulatePopulation initialPopulation

solution2 :: IO ()
solution2 = do
  initialPopulation <- parseInitialPopulation <$> readFile "inputs/day6/input1"
  print $ V.sum $ (!! 256) $ iterate simulatePopulation initialPopulation

