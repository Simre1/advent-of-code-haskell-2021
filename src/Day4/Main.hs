module Day4.Main where

import Data.Bool (bool)
import Data.IntSet (IntSet, member, insert, empty)
import Data.List.Split (chunksOf, splitOn)
import Data.Maybe (catMaybes)
import qualified Data.Vector as V

data Game = Game [Int] [V.Vector (V.Vector Int)] IntSet

parseInput :: String -> Game
parseInput string =
  let l = lines string
      numbers = fmap read $ splitOn "," $ head l
      fields = fmap (fmap read . words) . tail <$> chunksOf 6 (drop 1 l)
   in Game numbers (processField <$> fields) empty
  where
    processField :: [[Int]] -> V.Vector (V.Vector Int)
    processField fields = V.fromList . flip fmap fields $ \row -> V.fromList row

updateGame :: Game -> (Game, Int)
updateGame (Game numbers' fields drawnNumbers') = (Game numbers fields drawnNumbers, nextNumber)
  where
    (nextNumber : numbers) = numbers'
    drawnNumbers = insert nextNumber drawnNumbers'

checkField :: IntSet -> Int -> V.Vector (V.Vector Int) -> Bool
checkField drawnNumbers lastNumber field =
  let indices = (,) <$> [0 .. 4] <*> [0 .. 4]
      maybeIndex = safeHead $ catMaybes $ flip fmap indices $ \(i, j) -> bool Nothing (Just (i, j)) $ (field V.! i) V.! j == lastNumber
   in case maybeIndex of
        Nothing -> False
        Just (r, c) ->
          let rowWin = V.and $ fmap (`member` drawnNumbers) $ field V.! r
              columnWin = and $ fmap ((`member` drawnNumbers) . (V.! c)) $ (field V.!) <$> [0 .. 4]
           in rowWin || columnWin

calculateScore :: IntSet -> Int -> V.Vector (V.Vector Int) -> Int
calculateScore drawnNumbers lastNumber field = (* lastNumber) $ V.sum $ V.sum . fmap (\i -> if member i drawnNumbers then 0 else i) <$> field

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : xs) = Just x

solution1 :: IO ()
solution1 = do
  initialGame <- parseInput <$> readFile "inputs/day4/input1"
  let loop game =
        let (nextGame@(Game _ fields drawnNumbers), lastNumber) = updateGame game
            maybeWin = safeHead $ dropWhile (not . checkField drawnNumbers lastNumber) fields
         in maybe (loop nextGame) (calculateScore drawnNumbers lastNumber) maybeWin
  print $ loop initialGame

solution2 :: IO ()
solution2 = do
  initialGame <- parseInput <$> readFile "inputs/day4/input1"
  let loop game =
        let (Game numbers fields drawnNumbers, lastNumber) = updateGame game
            noWinFields = filter (not . checkField drawnNumbers lastNumber) fields
         in if null noWinFields
               then calculateScore drawnNumbers lastNumber $ head fields
               else loop $ Game numbers noWinFields drawnNumbers
  print $ loop initialGame
