module Day11.Main where

import Control.Arrow (Arrow (first, second))
import Control.Monad (forM_)
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.State (execStateT, gets, modify)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

parseInput :: String -> M.Map (Int, Int) Int
parseInput string =
  M.fromList . mconcat $
    zipWith parseRow [0 .. length (lines string) -1] (lines string)
  where
    parseRow rowIndex row = zipWith (\e columnIndex -> ((rowIndex, columnIndex), read [e])) row [0 .. length row -1]

step :: IORef Int -> M.Map (Int, Int) Int -> IO (M.Map (Int, Int) Int)
step flashCount initial = fmap fst $
  flip execStateT (initial, S.empty) $ do
    gets fst >>= M.foldlWithKey (\a k v -> a >> update k v) (pure ())
    let loop = do
          isEmpty <- S.null <$> gets snd
          if isEmpty
            then pure ()
            else do
              flashes <- gets snd
              modify $ second $ const S.empty
              forM_ flashes flash
              forM_ flashes updateNeighbors
              loop
    loop
  where
    flash pos = do
      lift $ modifyIORef flashCount succ
      modify $ first $ M.insert pos 0
    scheduleFlash pos = modify $ second $ S.insert pos
    update pos val
      | val >= 9 = scheduleFlash pos
      | otherwise = modify $ first $ M.insert pos (succ val)
    updateNeighbors (row, column) = do
      map <- gets fst
      let neighbors = mapMaybe (\pos -> (pos,) <$> M.lookup pos map) [(row + r, column + c) | r <- [-1 .. 1], c <- [-1 .. 1], (r, c) /= (0, 0)]
      forM_ neighbors $ \(pos, val) ->
        if
            | val == 0 -> pure ()
            | val >= 9 -> scheduleFlash pos
            | otherwise -> modify $ first $ M.adjust (+ 1) pos

solution1 :: IO ()
solution1 = do
  initialState <- parseInput <$> readFile "inputs/day11/test"
  flashCount <- newIORef 0
  let loop 0 s = pure s
      loop n s = step flashCount s >>= loop (n -1)
  loop 100 initialState
  readIORef flashCount >>= print
  pure ()

solution2 :: IO ()
solution2 = do
  initialState <- parseInput <$> readFile "inputs/day11/input1"
  flashCount <- newIORef 0
  let loop s n = do
        s' <- step flashCount s
        if and . fmap (== 0) $ s'
          then pure (succ n)
          else loop s' (succ n)
  loop initialState 0 >>= print
  pure ()
