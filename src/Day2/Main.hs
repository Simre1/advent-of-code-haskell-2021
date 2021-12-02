module Day2.Main where

data Direction = Down | Forward | Up

data Command = Command Direction Int

parse :: String -> [Command]
parse = fmap parseCommand . lines
  where 
    parseCommand s = 
      case break (== ' ') s of
        ("forward", x) -> Command Forward (read x)
        ("down", x) -> Command Down (read x)
        ("up", x) -> Command Up (read x)
        _ -> error "command unknown"

drive :: [Command] -> (Int, Int)
drive = foldl go (0,0)
  where 
    go (h,d) = \case
      Command Down a -> (h + a, d)
      Command Up a -> (h - a, d)
      Command Forward a -> (h, d + a)

drive2 :: [Command] -> (Int, Int, Int)
drive2 = foldl go (0,0,0)
  where 
    go (aim, h, d) = \case
      Command Down a -> (aim + a, h, d)
      Command Up a -> (aim - a, h, d)
      Command Forward a -> (aim, h + a, d + aim * a)

solution1 :: IO ()
solution1 = do
  commands <- parse <$> readFile "inputs/day2/input1"
  print $ uncurry (*) $ drive commands

solution2 :: IO ()
solution2 = do
  commands <- parse <$> readFile "inputs/day2/input1"
  print $  (\(_,h,d) -> h * d) $ 
    drive2 commands
