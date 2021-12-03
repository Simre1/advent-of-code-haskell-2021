module Day3.Main where

import Data.Foldable
import Data.Char (digitToInt)

solution1 :: IO ()
solution1 = do
  i <- lines <$> readFile "inputs/day3/input1"
  let (gamma,epsilon) = countGammaEpsilon i
  print $ toDec epsilon * toDec gamma
  pure ()

solution2 :: IO ()
solution2 =do
  i <- lines <$> readFile "inputs/day3/input1"
  let oxygen = countBit countOxygen i
  let scrubber = countBit countScrubber i
  print $ toDec oxygen * toDec scrubber
  pure () 

countGammaEpsilon :: [String] -> (String, String)
countGammaEpsilon numbers =
  let _N = length $ head numbers
      l = length numbers
      getMostCommon n = 
        (\i -> if i > l `quot` 2 then '1' else '0') $ 
          sum
            (fmap (\s -> read [s !! n]) numbers)
      mostCommon = getMostCommon <$> [0 .. _N -1]
   in (mostCommon, (\c -> if c == '0' then '1' else '0') <$> mostCommon)

countBit :: (Int -> Int -> Bool) -> [String] -> String
countBit f numbers =
  let _N = length $ head numbers
      bits = foldl' (\ns n -> if length ns == 1 then ns else filter (\number -> number !! n == getBit f n ns) ns) numbers [0 .. _N -1]
   in head bits

getBit f n ns = 
  (\i -> if f i (length ns) then '1' else '0') $ 
    sum
      (fmap (\s -> read [s !! n]) ns)

countOxygen i l = i > l `quot` 2 || even l && i >= l `quot` 2

countScrubber i l = i < l `quot` 2 || odd l && i <= l `quot` 2

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

