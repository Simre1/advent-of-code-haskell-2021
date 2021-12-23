module Day23.Main where

import Algorithm.Search (aStar)
import Data.Foldable (Foldable (foldl'), forM_)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromMaybe)
import Linear.V2 (V2 (..))

data Cell = Empty | A | B | C | D deriving (Eq, Ord, Show)

parseInput :: String -> M.Map (V2 Int) Cell
parseInput str =
  let hallwayAndRooms = take 5 . drop 1 $ lines str
   in foldl' (\m (x, y, r) -> maybe id (M.insert (V2 x y)) (readCell r) m) M.empty $
        concat $ zipWith (\y row -> fmap (\(x, v) -> (x, y, v)) row) [0 ..] $ fmap (zip [0 ..]) hallwayAndRooms

readCell :: Char -> Maybe Cell
readCell 'A' = Just A
readCell 'B' = Just B
readCell 'C' = Just C
readCell 'D' = Just D
readCell '.' = Just Empty
readCell _ = Nothing

writeCell :: Cell -> Char
writeCell A = 'A'
writeCell B = 'B'
writeCell C = 'C'
writeCell D = 'D'
writeCell Empty = '.'

newtype State = State {roomMap :: M.Map (V2 Int) Cell} deriving (Show, Eq, Ord)

printState :: State -> IO ()
printState (State m) = forM_ [0 .. 4] $ \y -> do
  forM_ [1 .. 11] $ \x ->
    putChar $ maybe '#' writeCell $ M.lookup (V2 x y) m
  putChar '\n'

findConfiguration :: M.Map (V2 Int) Cell -> Maybe (Int, [State])
findConfiguration map = aStar moves cost estimate isGoal $ State map

moves :: State -> [State]
moves (State m) =
  M.foldlWithKey' (\s k -> (s ++) . genMovesForCell m k) [] m

cost :: State -> State -> Int
cost (State m1) (State m2) = findCost m1 m2 $ M.keys m1

estimate :: State -> Int
estimate (State m) = M.foldlWithKey' (\a k -> (+ a) . estimateForCell k) 0 m

isGoal :: State -> Bool
isGoal = (== 0) . estimate

genMovesForCell :: M.Map (V2 Int) Cell -> V2 Int -> Cell -> [State]
genMovesForCell m pos@(V2 x y) cell =
  case cell of
    Empty -> []
    amphipod ->
      let (roomX, _) = cellInfo cell
          clear positions = all (== Empty) $ (m M.!) <$> positions
          checkRoom y c = M.lookup (V2 roomX y) m == c
          canEnterRoom y' = all (\a -> checkRoom a (Just Empty)) [1 .. y'] && all (\a -> checkRoom a (Just cell) || checkRoom a Nothing) [y' + 1 .. 4]
          findPathInHallway x =
            let f l = takeWhile (\(_, c) -> c == Empty) $ (\p -> (p, m M.! V2 p 0)) <$> filter (`notElem` [3, 5, 7, 9]) l
             in f [x -1, x -2 .. 1] ++ f [x + 1, x + 2 .. 11]
          goOutside =
            if clear [V2 x y' | y' <- [0 .. y -1]]
              then (\(p, c) -> State $ M.insert pos Empty $ M.insert (V2 p 0) cell m) <$> findPathInHallway x
              else []
          findRoom =
            if clear $ let r = [V2 x' 0 | x' <- if x <= roomX then [x + 1 .. roomX] else [roomX, roomX + 1 .. x -1]] in r
              then
                if
                    | canEnterRoom 4 -> [State $ M.insert pos Empty $ M.insert (V2 roomX 4) cell m]
                    | canEnterRoom 3 -> [State $ M.insert pos Empty $ M.insert (V2 roomX 3) cell m]
                    | canEnterRoom 2 -> [State $ M.insert pos Empty $ M.insert (V2 roomX 2) cell m]
                    | canEnterRoom 1 -> [State $ M.insert pos Empty $ M.insert (V2 roomX 1) cell m]
                    | otherwise -> []
              else []
       in if
              | roomX == x && all (\y' -> checkRoom y' (Just cell) || checkRoom y' Nothing) [y .. 4] -> []
              | y == 0 -> findRoom
              | otherwise -> goOutside

cellInfo :: Cell -> (Int, Int)
cellInfo = \case
  A -> (3, 1)
  B -> (5, 10)
  C -> (7, 100)
  D -> (9, 1000)
  Empty -> (0, 0)

estimateForCell :: V2 Int -> Cell -> Int
estimateForCell (V2 x y) cell =
  let (roomX, c) = cellInfo cell
      xDiff = abs (x - roomX)
      yCost
        | xDiff /= 0 = y * c
        | otherwise = if y == 0 then c else 0
   in xDiff * c + yCost

findCost :: M.Map (V2 Int) Cell -> M.Map (V2 Int) Cell -> [V2 Int] -> Int
findCost m1 m2 ps =
  let [(V2 x1 y1, c), (V2 x2 y2, _)] = go ps
   in snd (cellInfo c) * (abs (x2 - x1) + abs (y2 - y1))
  where
    go [] = []
    go (p : ps) =
      let c1 = m1 M.! p
          c2 = m2 M.! p
       in if c1 /= c2
            then (if c1 == Empty then (p, c2) else (p, c1)) : go ps
            else go ps

solution1 :: IO ()
solution1 = do
  input <- parseInput <$> readFile "inputs/day23/input1"
  print $ fst <$> findConfiguration input

solution2 :: IO ()
solution2 = do
  input <- parseInput <$> readFile "inputs/day23/input2"
  print $ fst <$> findConfiguration input
