module Day13.Main where
import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.Foldable (Foldable(foldl'))
import Control.Monad ( forM_ )

parseFile :: String -> (S.Set (Int, Int), [(Char, Int)])
parseFile string =
  let [paper, instructions'] = splitOn "\n\n" string
      dots = S.fromList $ (\[x, y] -> (read x, read y)) . splitOn "," <$> lines paper
      instructions = (\[a, i] -> (head a, read i)) . splitOn "=" . drop 11 <$> lines instructions'
   in (dots, instructions)

foldPaper :: S.Set (Int, Int) -> (Char, Int) -> S.Set (Int, Int)
foldPaper set (dim, i) = foldMap transform set
  where
    transform (x, y) =
      let flip a o = case compare a i of
            EQ -> S.empty
            LT -> S.singleton $ o a
            GT -> S.singleton $ o (2 * i - a)
       in case dim of
            'x' -> flip x (,y)
            _ -> flip y (x,)

solution1 :: IO ()
solution1 = do
  (dots, instructions) <- parseFile <$> readFile "inputs/day13/input1"
  print $ S.size $ foldPaper dots $ head instructions
  pure ()

solution2 :: IO ()
solution2 = do
  (dots, instructions) <- parseFile <$> readFile "inputs/day13/input1"
  let foldedPaperDots = foldl' foldPaper dots instructions
      (maximumW, maximumH) = foldl (\(accX, accY) (x,y) -> (max x accX, max y accY)) (0, 0) foldedPaperDots
  forM_ [0..maximumH] $ \y -> do
    forM_ [0..maximumW] $ \x ->
      putChar $ if S.member (x,y) foldedPaperDots 
                   then 'O' 
                   else ' '
    putChar '\n'
