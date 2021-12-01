module Day1.Main where

import Data.Foldable (foldl')

solution1 :: IO ()
solution1 = readFile "inputs/day1/input" >>= (print . countIncrease . parse)

solution2 :: IO ()
solution2 = readFile "inputs/day1/input" >>= (print . countIncrease . slidingWindow . parse)
 
parse :: String -> [Int]
parse = fmap read . lines

slidingWindow :: [Int] -> [Int]
slidingWindow (x:y:z:r) = (x+y+z) : slidingWindow (y:z:r)
slidingWindow _ = []

countIncrease :: [Int] -> Int
countIncrease = snd . foldl' go (-100,-1)
  where 
    go (d,c) i = 
      let newC = if i > d then succ c else c
      in (i, newC)
