module Day20.Main where

import Control.Arrow (Arrow (first))
import Data.Char (digitToInt)
import Data.Foldable
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Debug.Trace (traceShowId)
import Linear.V2

data Image = Image (M.Map (V2 Int) Char) Char

type Algorithm = V.Vector Char

parseInput :: String -> (Algorithm, Image)
parseInput str =
  let [algo, img] = splitOn "\n\n" str
      imageMap = M.fromList $ concatMap (\(r, v) -> first (V2 r) <$> zip [0 ..] v) $ zip [0 ..] $ lines img
   in (V.fromList algo, Image imageMap '.')

printImage :: Image -> IO ()
printImage (Image image _) = forM_ [minR .. maxR] $ \r -> do
  forM_ [minC .. maxC] $ \c ->
    putChar $ fromMaybe '.' $ M.lookup (V2 r c) image
  putChar '\n'
  where
    (V2 minR minC) = fst $ M.findMin image
    (V2 maxR maxC) = fst $ M.findMax image

getPixel :: Image -> V2 Int -> Char
getPixel (Image image d) i = fromMaybe d $ M.lookup i image

indices :: Image -> [V2 Int]
indices (Image image _) = [V2 r c | r <- [minR - 1 .. maxR + 1], c <- [minC - 1 .. maxC + 1]]
  where
    (V2 minR minC) = fst $ M.findMin image
    (V2 maxR maxC) = fst $ M.findMax image

binaryToInt :: String -> Int
binaryToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

convolutionFields :: V2 Int -> [V2 Int]
convolutionFields v = [v + V2 r c | r <- [-1 .. 1], c <- [-1 .. 1]]

applyAlgorithm :: Algorithm -> Image -> Image
applyAlgorithm algorithm fullImage@(Image image d) =
  let image' = foldl' applyOnSinglePixel image $ indices fullImage
      d' = algorithm V.! (if d == '#' then 511 else 0)
   in Image image' d'
  where
    applyOnSinglePixel image' i =
      let algoIndex = binaryToInt $ (\c -> if c == '#' then '1' else '0') . getPixel fullImage <$> convolutionFields i
       in M.insert i (algorithm V.! algoIndex) image'

countLitPixels :: Image -> Int
countLitPixels (Image _ '#') = error "Infinite amount of lit pixels"
countLitPixels (Image image _) = M.foldl' (\acc v -> acc + if v == '#' then 1 else 0) 0 image

solution1 :: IO ()
solution1 = do
  (algo, image) <- parseInput <$> readFile "inputs/day20/input1"
  print $ countLitPixels $ applyAlgorithm algo $ applyAlgorithm algo image

solution2 :: IO ()
solution2 = do
  (algo, image) <- parseInput <$> readFile "inputs/day20/input1"
  print $ countLitPixels $ iterate (applyAlgorithm algo) image !! 50
