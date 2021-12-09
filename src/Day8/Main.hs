module Day8.Main where

import Data.Functor ((<&>))
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import SAT.MiniSat ( solve, Formula(All, (:&&:), ExactlyOne, Var) )

parseInput :: String -> [([String], [String])]
parseInput input =
  lines input <&> \l ->
    let [numbers, outputNumbers] = splitOn "|" l
     in (words numbers, words outputNumbers)

solution1 :: IO ()
solution1 = do
  input <- parseInput <$> readFile "inputs/day8/test"
  print $ length $ mconcat $ filter (\s -> length s `elem` [2, 3, 4, 7]) . snd <$> input

solution2 :: IO ()
solution2 = do
  input <- parseInput <$> readFile "inputs/day8/input1"
  print . sum $ decodeLine <$> input

decodeLine :: ([String], [String]) -> Int
decodeLine (numbers, output) =
  let formula = mapUnknownToReal :&&: All (formulaForInputNumber <$> numbers)
      Just solution = solve formula
      key = M.fromList $ extractSolution solution
      decode n = signalsToNumber M.! S.fromList ((key M.!) <$> n)
   in read $ decode <$> output

signalsToNumber :: M.Map (S.Set Char) Char
signalsToNumber = M.fromList $ zip (S.fromList <$> numberCodes) ['0' .. '9']

extractSolution :: M.Map String Bool -> [(Char, Char)]
extractSolution = fmap (\[a, b] -> (a, b)) . M.keys . M.filter id

formulaForInputNumber :: String -> Formula String
formulaForInputNumber encodedNumber =
  let possibleNumbers = filter (\n -> length n == length encodedNumber) numberCodes
   in ExactlyOne $
        flip fmap possibleNumbers $ \n -> All $
          flip fmap encodedNumber $ \u ->
            ExactlyOne $ flip fmap n $ \r -> Var (u : [r])

numberCodes :: [String]
numberCodes = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

mapUnknownToReal :: Formula String
mapUnknownToReal =
  All
    [ makeMapping 'a',
      makeMapping 'b',
      makeMapping 'c',
      makeMapping 'd',
      makeMapping 'e',
      makeMapping 'f',
      makeMapping 'g'
    ]
  where
    makeMapping cr = ExactlyOne $ (\cu -> Var (cu : [cr])) <$> ['a' .. 'g']
