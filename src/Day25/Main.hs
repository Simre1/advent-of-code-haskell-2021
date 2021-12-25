module Day25.Main where

import Control.Applicative ( Applicative(liftA2) )
import Control.Arrow ( Arrow(second) )
import Control.Monad ( forM_ )
import Control.Monad.ST ( runST )
import qualified Data.Massiv.Array as A
import qualified Data.Massiv.Array.Mutable as AM

parseInput :: String -> A.Array A.U A.Ix2 Int
parseInput str =
  let rows = zip [0 ..] $ lines str
      cols = concatMap (\(r, rs) -> fmap (\(c, v) -> (r, c, v)) rs) $ fmap (second $ zip [0 ..]) rows
      rowLength = length rows
      colLength = length $ snd $ head rows
   in snd $
        A.createArrayST (A.Sz2 rowLength colLength) $ \marray ->
          forM_ cols $ \(r, c, v) ->
            let i = case v of '>' -> 1; 'v' -> 2; _ -> 0
             in AM.write marray (A.Ix2 r c) i

stepType :: Int -> A.Array A.U A.Ix2 Int -> A.Array A.U A.Ix2 Int
stepType type_ seaFloor = runST $ do
  marray <- A.thawS seaFloor
  forM_ (liftA2 A.Ix2 [0 .. rs -1] [0 .. cs -1]) $ \pos -> do
    let nextPosition = circleIndex' (pos + direction)
    if (seaFloor A.! pos) == type_ && (seaFloor A.! nextPosition) == 0
      then do
        AM.write marray pos 0
        AM.write marray nextPosition type_
        pure ()
      else pure ()
  A.freezeS marray
  where
    (A.Sz2 rs cs) = A.size seaFloor
    direction = case type_ of
      1 -> A.Ix2 0 1
      2 -> A.Ix2 1 0
      _ -> A.Ix2 0 0
    circleIndex' = circleIndex seaFloor

step :: A.Array A.U A.Ix2 Int -> A.Array A.U A.Ix2 Int
step = stepType 2 . stepType 1

circleIndex :: A.Size r => A.Array r A.Ix2 e -> A.Ix2 -> A.Ix2
circleIndex arr (A.Ix2 r c) = A.Ix2 (r `mod` rs) (c `mod` cs)
  where
    (A.Sz2 rs cs) = A.size arr

solution1 :: IO ()
solution1 = do
  input <- parseInput <$> readFile "inputs/day25/input1"
  let loop !seaFloor !n =
        let next = step seaFloor
         in if next == seaFloor
              then succ n
              else loop next (succ n)
  print $ loop input 0

solution2 :: IO ()
solution2 = pure ()
