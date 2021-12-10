module Day10.Main where

import Control.Applicative ( Alternative((<|>)) )
import Data.Either (fromRight)
import Data.Functor ( ($>), (<&>) )
import Data.Maybe (catMaybes, mapMaybe)
import Data.Void ( Void )
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec as T
import qualified Text.Megaparsec.Char as P
import Data.Foldable ( Foldable(foldl') )
import Data.List ( sort )

data Result = Success | Incomplete String | Corrupted Char deriving (Eq, Ord, Show)

type Parser = T.Parsec Void String

parse :: String -> T.Parsec e String a -> [a]
parse input parser = fmap (fromRight undefined . P.parse parser "line") . lines $ input

parser :: Parser Result
parser = chunks
  where
    chunks :: Parser Result
    chunks = chunk '(' ')' <|> chunk '[' ']' <|> chunk '{' '}' <|> chunk '<' '>' <|> (pure () $> Success)
    chunk :: Char -> Char -> Parser Result
    chunk s e = do
      P.try $ P.char s
      r <- chunks
      case r of
        Success -> (P.eof $> Incomplete [e]) <|> (P.char e >> chunks) <|> (P.satisfy (const True) <&> Corrupted)
        Incomplete s -> pure $ Incomplete (s++[e])
        _ -> pure r

solution1 :: IO ()
solution1 = do
  input <- readFile "inputs/day10/input1"
  let result = parse input parser
  print $
    sum $
      fmap
        (\case Corrupted c -> toPoints c; _ -> 0)
        result
  where 
    toPoints ')' = 3
    toPoints ']' = 57
    toPoints '}' = 1197
    toPoints '>' = 25137
    toPoints _ = 0

solution2 :: IO ()
solution2 = do
  input <- readFile "inputs/day10/input1"
  let result = parse input parser
      scores = foldl' (\score c -> score * 5 + toPoints c) 0 <$> mapMaybe (\case Incomplete c -> Just c; _ -> Nothing) result
  print $ sort scores !! (length scores `quot` 2)

    where 
    toPoints ')' = 1
    toPoints ']' = 2
    toPoints '}' = 3
    toPoints '>' = 4
    toPoints _ = 0


