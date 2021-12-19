module Day18.Main where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P
import Debug.Trace (traceShowId)

data Number = Pair Number Number | Lit Int deriving (Show, Eq)

parse :: String -> [Number]
parse = either (error . P.errorBundlePretty) id . P.parse p "target value"
  where
    p :: P.Parsec Void String [Number]
    p = many $ number >>= (<$ (void P.newline <|> P.eof))
    number =
      pair <|> lit
    lit = Lit <$> P.decimal
    pair = do
      P.char '['
      n1 <- number
      P.char ','
      n2 <- number
      P.char ']'
      pure $ Pair n1 n2

addNumbers :: Number -> Number -> Number
addNumbers n1 n2 = reduceNumber $ Pair n1 n2

reduceNumber :: Number -> Number
reduceNumber n = maybe n reduceNumber $ explode n <|> split n

fromLit :: Number -> Int
fromLit (Lit n) = n
fromLit _ = error "Tried to extract Lit from Pair"

explode :: Number -> Maybe Number
explode = go (\_ n _ -> Just n) 0
  where
    go f _ (Lit a) = Nothing
    go f d (Pair a b)
      | d < 4 =
        go (\l n r -> f l (Pair n $ addLeftMost r b) 0) (succ d) a
          <|> go (\l n r -> f 0 (Pair (addRightMost l a) n) r) (succ d) b
      | otherwise = f (fromLit a) (Lit 0) (fromLit b)
    addLeftMost 0 n = n
    addLeftMost i (Lit n) = Lit $ i + n
    addLeftMost i (Pair a b) = Pair (addLeftMost i a) b
    addRightMost 0 n = n
    addRightMost i (Lit n) = Lit $ i + n
    addRightMost i (Pair a b) = Pair a (addRightMost i b)

split :: Number -> Maybe Number
split = go
  where 
    go (Lit i)
      | i > 9 = Just $ Pair (Lit (i `quot` 2)) (Lit . abs $ i `div` (-2))
      | otherwise = Nothing
    go (Pair a b) =
      flip Pair b <$> go a <|> Pair a <$> go b

magnitude :: Number -> Int
magnitude (Lit a) = a
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b

solution1 :: IO ()
solution1 = do
  input <- parse <$> readFile "inputs/day18/input1"
  print $ magnitude $ foldl1 addNumbers input

solution2 :: IO ()
solution2 = do
  input <- parse <$> readFile "inputs/day18/input1"
  print $ maximum $ [max (magnitude (addNumbers n1 n2)) (magnitude (addNumbers n2 n1)) | n1 <- input, n2 <- input, n1 /= n2]
