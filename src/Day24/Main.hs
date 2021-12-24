module Day24.Main where

import Data.Foldable (Foldable (foldl'))
import Data.SBV
  ( EqSymbolic ((.==)),
    OptimizeResult,
    OptimizeStyle (Lexicographic),
    OrdSymbolic ((.<), (.>)),
    SDivisible (sMod, sQuot),
    SInt64,
    Symbolic,
    constrain,
    maximize,
    minimize,
    mkExistVars,
    oneIf,
    optimize,
    sAll,
    (.&&),
  )
import Linear.V4 (R1 (_x), R2 (_y), R3 (_z), R4 (_w), V4 (..))
import Optics.Core (lensVL, (&), (.~), (^.))

data Register = X | Y | Z | W
  deriving (Show, Eq, Ord)

data Value = Literal Int | Register Register
  deriving (Show, Eq, Ord)

data Instruction
  = Input Register
  | Add Register Value
  | Multiply Register Value
  | Divide Register Value
  | Modulo Register Value
  | Equal Register Value
  deriving (Show, Eq, Ord)

readInput :: String -> [Instruction]
readInput = fmap parseInstruction . lines
  where
    parseInstruction = \case
      ['i', 'n', 'p', ' ', r] -> Input (parseRegister r)
      ('a' : 'd' : 'd' : ' ' : r : ' ' : v) -> Add (parseRegister r) (parseValue v)
      ('m' : 'u' : 'l' : ' ' : r : ' ' : v) -> Multiply (parseRegister r) (parseValue v)
      ('d' : 'i' : 'v' : ' ' : r : ' ' : v) -> Divide (parseRegister r) (parseValue v)
      ('m' : 'o' : 'd' : ' ' : r : ' ' : v) -> Modulo (parseRegister r) (parseValue v)
      ('e' : 'q' : 'l' : ' ' : r : ' ' : v) -> Equal (parseRegister r) (parseValue v)
      i -> error $ "unknown instruction: " ++ i
    parseValue s
      | head s `elem` ("xyzw" :: String) = Register $ parseRegister (head s)
      | otherwise = Literal $ read s
    parseRegister = \case
      'x' -> X
      'y' -> Y
      'z' -> Z
      'w' -> W
      r -> error $ "unknown register: " ++ [r]

executeProgram :: forall a. Num a => (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> [a] -> [Instruction] -> V4 a
executeProgram equal div mod input instructions = snd $ foldl' executeInstruction (input, V4 0 0 0 0) instructions
  where
    executeInstruction :: ([a], V4 a) -> Instruction -> ([a], V4 a)
    executeInstruction (input, registers) = \case
      Input r ->
        let registers' = registers & accessRegister r .~ head input
            input' = tail input
         in (input', registers')
      Add r v -> (input, operation registers r v (+))
      Divide r v -> (input, operation registers r v div)
      Modulo r v -> (input, operation registers r v mod)
      Multiply r v -> (input, operation registers r v (*))
      Equal r v -> (input, operation registers r v equal)
    operation :: V4 a -> Register -> Value -> (a -> a -> a) -> V4 a
    operation registers r v op =
      let v1 = registers ^. accessRegister r
          v2 = case v of Literal i -> fromIntegral i; Register r' -> registers ^. accessRegister r'
       in registers & accessRegister r .~ op v1 v2
    accessRegister = \case
      X -> lensVL _x
      Y -> lensVL _y
      Z -> lensVL _z
      W -> lensVL _w

findModelNumber :: (String -> SInt64 -> Symbolic ()) -> [Instruction] -> IO OptimizeResult
findModelNumber f input = optimize Lexicographic $ do
  vars :: [SInt64] <- mkExistVars 14
  constrain $ flip sAll vars $ \n -> n .> 0 .&& n .< 10
  let res = executeProgram (\a b -> oneIf (a .== b)) sQuot sMod vars input
  constrain $ (res ^. lensVL _z) .== 0
  f "model number" $ foldl' (\acc a -> acc * 10 + a) 0 vars - 0x8000000000000000 -- I do not really know why sbv automatically adds 0x8000000000000000 to the result

solution1 :: IO ()
solution1 = do
  input <- readInput <$> readFile "inputs/day24/input1"
  findModelNumber maximize input >>= print

solution2 :: IO ()
solution2 = do
  input <- readInput <$> readFile "inputs/day24/input1"
  findModelNumber minimize input >>= print
