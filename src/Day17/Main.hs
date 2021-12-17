module Day17.Main where

import Control.Applicative (Alternative ((<|>)))
import Data.Char (isDigit)
import Data.Either (fromRight)
import Data.Functor (($>))
import Data.Void (Void)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as P

parseInput :: String -> ((Int, Int), (Int, Int))
parseInput = either (error . P.errorBundlePretty) id . P.parse p "target value"
  where
    p :: P.Parsec Void String ((Int, Int), (Int, Int))
    p = do
      P.takeWhile1P Nothing (not . isDigit)
      xLower <- decimal
      P.takeP Nothing 2
      xUpper <- decimal
      P.takeP Nothing 4
      yLower <- decimal
      P.takeP Nothing 2
      yUpper <- decimal
      pure ((xLower, xUpper), (yLower, yUpper))
    decimal = do
      sig <- P.char '-' $> negate <|> pure id
      sig <$> P.decimal

trickShotVelocity :: ((Int, Int), (Int, Int)) -> (Int, Int)
trickShotVelocity (xBounds, yBounds) = (minimalDX, maximalDY)
  where
    minimalDX = minimizeDX xBounds
    maximalDY = maximizeDY yBounds

maximizeDY :: (Int, Int) -> Int
maximizeDY (lower, upper) = abs lower - 1

minimizeDX :: (Int, Int) -> Int
minimizeDX (lower, upper) = go 0 0
  where
    go dx x
      | x < lower = go (succ dx) (succ dx + x)
      | x <= upper = dx
      | otherwise = error "Overshooting handling not considered"

allVelocities :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
allVelocities ((xLower, xUpper), (yLower, yUpper)) = [(vx, vy) | vx <- xs, vy <- ys, isSolution vx vy]
  where
    ys = [yLower .. negate yLower]
    xs = [minimizeDX (xLower, xUpper) .. xUpper]
    isSolution (fromIntegral -> vx) (fromIntegral -> vy) =
      let validNsForY = findValidNsForY vy
       in foldr (\n acc -> let x = calculateX vx n in acc || (x >= xLower && x <= xUpper)) False validNsForY
    calculateX :: Int -> Int -> Int
    calculateX vx n = sum $ take n [vx, vx -1 .. 1]
    findValidNsForY :: Int -> [Int]
    findValidNsForY vy = go vy 0 0
      where
        go vy y n
          | y >= yLower && y <= yUpper = n : go (pred vy) (y + vy) (succ n)
          | vy < 0 && y < yLower = []
          | otherwise = go (pred vy) (y + vy) (succ n)

highestY :: Int -> Int
highestY vy = sum [0 .. vy]

solution1 :: IO ()
solution1 = do
  input <- parseInput <$> readFile "inputs/day17/input1"
  print $ highestY $ snd $ trickShotVelocity input

solution2 :: IO ()
solution2 = do
  input <- parseInput <$> readFile "inputs/day17/input1"
  print $ length $ allVelocities input
  pure ()
