module Day21.Main where

import Control.Arrow (second)
import Data.Foldable ( Foldable(foldl') )
import Data.Functor ( (<&>) )
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M

data Player = One | Two deriving (Show, Eq, Ord)

data DeterministicDice = DeterministicDice Int Int deriving (Show)

data QuantumDice = QuantumDice deriving (Show)

data Game d = Game
  { p1Score :: Int,
    p2Score :: Int,
    p1Position :: Int,
    p2Position :: Int,
    pTurn :: Player,
    dice :: d
  }
  deriving (Show)

class Dice a where
  roll :: a -> [(Int, a)]

instance Dice DeterministicDice where
  roll (DeterministicDice r v) = let v' = succ v `mod` 100 in [(v', DeterministicDice (succ r) v')]

instance Dice QuantumDice where
  roll _ = zip [1, 2, 3] $ repeat QuantumDice

parseInput :: String -> (Int, Int)
parseInput = (\[a, b] -> (read a, read b)) . fmap (last . splitOn ": ") . lines

playTurn :: (Dice d) => Game d -> [Game d]
playTurn game =
  rolls <&> \(r1, r2, r3, d) ->
    let sumOfRolls = r1 + r2 + r3
        nextPosition = succ $ (pred position + sumOfRolls) `mod` 10
     in flipPlayer $ setPosition nextPosition $ updateScore (+ nextPosition) game {dice = d}
  where
    rolls = do
      (r1, d) <- roll (dice game)
      (r2, d') <- roll d
      (r3, d'') <- roll d'
      pure (r1, r2, r3, d'')
    (position, setPosition, updateScore, flipPlayer) = case pTurn game of
      One ->
        ( p1Position game,
          \p g -> g {p1Position = p},
          \s g -> g {p1Score = s (p1Score g)},
          \g -> g {pTurn = Two}
        )
      Two ->
        ( p2Position game,
          \p g -> g {p2Position = p},
          \s g -> g {p2Score = s (p2Score g)},
          \g -> g {pTurn = One}
        )

playerWon :: Int -> Game d -> Maybe Player
playerWon s g
  | p1Score g >= s = Just One
  | p2Score g >= s = Just Two
  | otherwise = Nothing

playGame1 :: (Dice d) => (Int, Int) -> d -> [(Game d, Player)]
playGame1 (p1Pos, p2Pos) d =
  let loop g = do
        g' <- playTurn g
        maybe (loop g') (\p -> [(g', p)]) $ playerWon 1000 g'
   in loop initialGame
  where
    initialGame =
      Game
        { p1Score = 0,
          p2Score = 0,
          p1Position = p1Pos,
          p2Position = p2Pos,
          pTurn = One,
          dice = d
        }

playGame2 :: (Int, Int) -> (Int, Int)
playGame2 (p1Pos, p2Pos) =
  let loop c g = do
        case M.lookup (p1Position g, p2Position g, p1Score g, p2Score g, pTurn g) c of
          Just r -> (r, c)
          Nothing -> case playerWon 21 g of
            Just p -> case p of One -> ((1, 0), c); Two -> ((0, 1), c)
            Nothing ->
              let (r, c') =
                    foldl'
                      (\((p1S, p2S), c') g' -> let ((a, b), c'') = loop c' g' in ((p1S + a, p2S + b), c''))
                      ((0 :: Int, 0 :: Int), c)
                      (playTurn g)
               in (r, M.insert (p1Position g, p2Position g, p1Score g, p2Score g, pTurn g) r c')
   in fst $ loop M.empty initialGame
  where
    initialGame =
      Game
        { p1Score = 0,
          p2Score = 0,
          p1Position = p1Pos,
          p2Position = p2Pos,
          pTurn = One,
          dice = QuantumDice
        }

calculateCode :: Player -> Game DeterministicDice -> Int
calculateCode p g =
  rolls * case p of
    One -> p2Score g
    Two -> p1Score g
  where
    DeterministicDice rolls _ = dice g

solution1 :: IO ()
solution1 = do
  (p1Pos, p2Pos) <- parseInput <$> readFile "inputs/day21/input1"
  let [(g, p)] = playGame1 (p1Pos, p2Pos) $ DeterministicDice 0 0
  print $ calculateCode p g

solution2 :: IO ()
solution2 = do
  (p1Pos, p2Pos) <- parseInput <$> readFile "inputs/day21/input1"
  let (p1,p2) = playGame2 (p1Pos, p2Pos)
  print $ max p1 p2
