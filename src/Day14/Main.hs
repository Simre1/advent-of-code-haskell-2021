module Day14.Main where

import Control.Arrow (second)
import Data.Foldable (Foldable (foldl'))
import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

parseInput :: String -> (M.Map String Char, String)
parseInput s =
  let [seqStr, rulesStr] = splitOn "\n\n" s
      seq = seqStr
      rules = M.fromList $ (\[l, [c]] -> (l, c)) . splitOn " -> " <$> lines rulesStr
   in (rules, seq)

step :: M.Map String Char -> String -> Int -> M.Map Char Int
step rules s = M.unionWith (+) (M.singleton (last s) 1) . snd . go M.empty s
  where
    go :: M.Map (Int, String) (M.Map Char Int) -> String -> Int -> (M.Map (Int, String) (M.Map Char Int), M.Map Char Int)
    go cache str 0 = (cache, foldl' (flip $ M.alter $ pure . succ . fromMaybe 0) M.empty $ init str)
    go cache [b] i = (cache, M.empty)
    go cache (a : b : r) i = case M.lookup (i, a : [b]) cache of
      Just found -> let (newCache, nextCounts) = go cache (b : r) i in (newCache, M.unionWith (+) found nextCounts)
      Nothing ->
        let (newCache, intermediateCounts) = case M.lookup (a : [b]) rules of
              Nothing -> (cache, M.fromList [(a, 1), (b, 1)])
              Just c -> go cache (a : c : [b]) (pred i)
         in second (M.unionWith (+) intermediateCounts) (go (M.insert (i, a : [b]) intermediateCounts newCache) (b : r) i)
    go cache _ _ = (cache, M.empty)

solution1 :: IO ()
solution1 = do
  (rules, input) <- parseInput <$> readFile "inputs/day14/input1"
  let occurences = sort $ snd <$> M.toList (step rules input 10)
  print $ last occurences - head occurences

solution2 :: IO ()
solution2 = do
  (rules, input) <- parseInput <$> readFile "inputs/day14/input1"
  let occurences = sort $ snd <$> M.toList (step rules input 40)
  print $ last occurences - head occurences
