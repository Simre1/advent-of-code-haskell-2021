module Day12.Main where

import Algorithm.Search (dfsM)
import Control.Monad.Trans.State (State, execState, modify)
import Data.Char (isLower)
import Data.List (foldl')
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S

type CaveSystem = M.Map String [String]

parseInput :: String -> CaveSystem
parseInput = foldl' parseLine M.empty . lines
  where
    parseLine m str = let [start, end] = splitOn "-" str in M.alter (\n -> pure $ start : fromMaybe [] n) end $ M.alter (\n -> pure $ end : fromMaybe [] n) start m

listAllPaths :: Bool -> CaveSystem -> [[String]]
listAllPaths visitTwice caveSystem = flip execState [] $ dfsM nextStates isGoal initialState
  where
    initialState = (S.empty, [], visitTwice, "start")
    isGoal _ = pure False
    nextStates :: (S.Set String, [String], Bool, String) -> State [[String]] [(S.Set String, [String], Bool, String)]
    nextStates (visited, path, canVisitTwice, currentNode) =
      if currentNode == "end"
        then modify (("end" : path) :) >> pure []
        else do
          let path' = currentNode : path
              visited' = if isLower (head currentNode) then S.insert currentNode visited else visited
              nextNodes =
                let next = filter (/= "start") $ fromMaybe [] $ M.lookup currentNode caveSystem
                 in if canVisitTwice
                      then ((False,) <$> filter (`S.member` visited) next) <> ((True,) <$> filter (`S.notMember` visited) next)
                      else (False,) <$> filter (`S.notMember` visited) next
           in pure $ (\(a, b) -> (visited', path', a, b)) <$> nextNodes

solution1 :: IO ()
solution1 = do
  caveSystem <- parseInput <$> readFile "inputs/day12/input1"
  print $ length $ listAllPaths False caveSystem

solution2 :: IO ()
solution2 = do
  caveSystem <- parseInput <$> readFile "inputs/day12/input1"
  print $ length $ listAllPaths True caveSystem
