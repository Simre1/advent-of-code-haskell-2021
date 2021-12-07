module Day7.Main where
import Data.List.Split (splitOn)
import Data.List

parseInput :: String -> [Int]
parseInput = fmap read . splitOn ","

findPositionMedian :: [Int] -> Int
findPositionMedian positions = sort positions !! (length positions `quot` 2)

calculateFuelSimple :: Int -> [Int] -> Int
calculateFuelSimple destinationPosition crabs = sum $ fmap (abs . (+ negate destinationPosition)) crabs

calculateFuelBinom :: Int -> [Int] -> Int
calculateFuelBinom destinationPosition crabs = sum $ fmap (binom . abs . (+ negate destinationPosition)) crabs
  where binom x = x*(x+1)`quot`2 

findPositionMean :: [Int] -> (Int, Int)
findPositionMean positions = (m, m')
  where
    s = sum positions
    l = length positions
    m = s `quot` l
    m' = m + 1

solution1 :: IO ()
solution1 = do
  positions <- parseInput <$> readFile "inputs/day7/input1"
  print $ calculateFuelSimple (findPositionMedian positions) positions
  pure ()

solution2 :: IO ()
solution2 = do
  positions <- parseInput <$> readFile "inputs/day7/input1"
  let (p1,p2) = findPositionMean positions
  let (f1,f2) = (calculateFuelBinom p1 positions, calculateFuelBinom p2 positions)
  print $ min f1 f2
  pure ()


