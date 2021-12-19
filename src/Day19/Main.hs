module Day19.Main where

import Control.Applicative ( Applicative(liftA2) )
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Linear.V3 ( V3(..) )
import qualified Data.Set as S

data Coords = X | Y | Z deriving (Show, Eq, Enum, Ord)

data Direction a = Forward a | Backward a deriving (Show, Eq, Ord)

data Orientation = Orientation (V3 (Direction Coords)) deriving (Show, Eq, Ord)

data Normalised = Normalised Int Int Int

data Scanner = Scanner (V3 Int) [V3 Int] deriving (Show, Eq)

readInput :: String -> [[V3 Int]]
readInput str =
  let scanners = splitOn "\n\n" str
   in fmap ((\[a, b, c] -> V3 a b c) . fmap read . splitOn ",") . tail . lines <$> scanners

applyOrientation :: Orientation -> V3 Int -> V3 Int
applyOrientation (Orientation (V3 dx dy dz)) (V3 x y z) = f dx x + f dy y + f dz z
  where
    f d n =
      let (m, c) = case d of
            Forward a -> (1, a)
            Backward a -> (-1, a)
       in case c of
            X -> V3 (m * n) 0 0
            Y -> V3 0 (m * n) 0
            Z -> V3 0 0 (m * n)

allOrientations :: [Orientation]
allOrientations =
  [ Orientation $ V3 (fx x) (fy y) (fz z)
    | fx <- directions,
      fy <- directions,
      fz <- directions,
      x <- coords,
      y <- coords,
      x /= y,
      z <- coords,
      y /= z,
      x /= z
  ]
  where
    coords = [X, Y, Z]
    directions = [Forward, Backward]

matchScanner :: Scanner -> [V3 Int] -> Maybe Scanner
matchScanner (Scanner pos readings) unknownReadings =
  case foldr fold (Left M.empty) allPossibilities of
    Left _ -> Nothing
    Right (position, orientation) -> Just $ Scanner position (applyOrientation orientation <$> unknownReadings)
  where
    fold :: (V3 Int, (V3 Int, Orientation)) -> Either (M.Map (V3 Int, Orientation) Int) (V3 Int, Orientation) -> Either (M.Map (V3 Int, Orientation) Int) (V3 Int, Orientation)
    fold _ (Right a) = Right a
    fold (r, (u, o)) (Left m) = case M.lookup (pos + r - u, o) m of
      Nothing -> Left $ M.insert (pos + r - u, o) 1 m
      Just i -> if i >= 11 then Right (pos + r - u, o) else Left $ M.insert (pos + r - u, o) (succ i) m
    allPossibilities = liftA2 (,) readings ((\u o -> (applyOrientation o u, o)) <$> unknownReadings <*> allOrientations)

findAllScanners :: Scanner -> [[V3 Int]] -> [Scanner]
findAllScanners scanner0 = go [scanner0]
  where
    go [] _ = []
    go scanners [] = scanners
    go (s : scanners) readings =
      let (foundScanners, undeterminedReadings) = findOverlapping s readings
       in s : go (scanners ++ foundScanners) undeterminedReadings
    findOverlapping s [] = ([], [])
    findOverlapping s (r : rs) =
      let (foundScanners, undeterminedReadings) = findOverlapping s rs
       in maybe (foundScanners, r : undeterminedReadings) (\f -> (f : foundScanners, undeterminedReadings)) (matchScanner s r)

solution1 :: IO ()
solution1 = do
  input <- readInput <$> readFile "inputs/day19/input1"
  let scanner0 = Scanner (V3 0 0 0) $ head input
      scanners = findAllScanners scanner0 $ tail input
  print $ S.size $ S.fromList $ foldMap (\(Scanner p readings) -> (p+) <$> readings) scanners


solution2 :: IO ()
solution2 = do
  input <- readInput <$> readFile "inputs/day19/input1"
  let scanner0 = Scanner (V3 0 0 0) $ head input
      scanners = findAllScanners scanner0 $ tail input
      positions = (\(Scanner pos _) -> pos) <$> scanners
  print $ maximum $ [sum $ abs (p2 - p1)| p1 <- positions, p2 <- positions]

