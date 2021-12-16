{-# language UndecidableInstances #-}

module Day16.Main where

import GHC.TypeLits (Symbol)
import qualified Text.Megaparsec as P
import Data.Foldable (foldl')
import Data.Char (digitToInt)
import Data.Functor ((<&>))
import Data.Either (fromRight)
import Control.Applicative (many)
import Control.Monad (replicateM)

hexToBinary :: Char -> String
hexToBinary = \case
  '0' -> "0000"
  '1' -> "0001"
  '2' -> "0010"
  '3' -> "0011"
  '4' -> "0100"
  '5' -> "0101"
  '6' -> "0110"
  '7' -> "0111"
  '8' -> "1000"
  '9' -> "1001"
  'A' -> "1010"
  'B' -> "1011"
  'C' -> "1100"
  'D' -> "1101"
  'E' -> "1110"
  'F' -> "1111"
  _ -> ""

binaryToInt :: String -> Int
binaryToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

hexStringToBinaryString :: String -> String
hexStringToBinaryString = foldMap hexToBinary

data Packet where
  Packet :: Show (PacketPayload a) => Int -> PacketType a -> PacketPayload a -> Packet

deriving instance Show Packet

type family PacketPayload (a :: Symbol) where
  PacketPayload "literal" = Int
  PacketPayload "operation" = [Packet]
  PacketPayload a = ()

data PacketType a where
  Literal :: PacketType "literal"
  Operation :: OperationType -> PacketType "operation"

data OperationType = Sum | Product | Minimum | Maximum | Greater | Less | Equal deriving Show

instance Show (PacketType a) where
  show Literal = "literal"
  show (Operation t) = "operation " ++ show t

type Parser = P.Parsec () String

newtype Exists k f = Exists (forall x. (forall (a :: k). f a -> x) -> x)

makeExists :: forall k1 (f :: k1 -> *) (a :: k1). f a -> Exists k1 f
makeExists a = Exists (\f -> f a)

runExists :: Exists k f -> (forall (a :: k). f a -> x) -> x
runExists (Exists r) = r

parser :: Parser Packet
parser = packet
  where
    version :: Parser Int
    version = binaryToInt <$> P.takeP (Just "version") 3
    packetType :: Parser (Exists Symbol PacketType)
    packetType = (binaryToInt <$> P.takeP (Just "packetType type") 3) <&> \case
      4 -> makeExists Literal
      0 -> makeExists $ Operation Sum
      1 -> makeExists $ Operation Product
      2 -> makeExists $ Operation Minimum
      3 -> makeExists $ Operation Maximum
      5 -> makeExists $ Operation Greater
      6 -> makeExists $ Operation Less
      7 -> makeExists $ Operation Equal
      _ -> error "Undefined packet type"
    literalNumber :: Parser Int
    literalNumber =
      let number = do
            (l:num) <- P.takeP (Just "single hex digit") 5
            case l of
              '1' -> (num ++) <$> number
              _ -> pure num
      in binaryToInt <$> number
    operationPayload :: Parser [Packet]
    operationPayload = do
      [m] <- P.takeP (Just "Mode") 1
      case m of
        '0' -> do
          l <- binaryToInt <$> P.takeP (Just "subpackets total length") 15
          subpacketBits <- P.takeP (Just "subpacket bits") l
          let subPackets = fromRight (error "Could not parse sub packets in mode 0") $ P.parse (many parser >>= (<$ P.eof)) "subpackets" subpacketBits
          pure subPackets
        _ -> do
          l <- binaryToInt <$> P.takeP (Just "subpackets total length") 11
          replicateM l packet
    packet :: Parser Packet
    packet = do
      packetVersion <- version
      packetType <- packetType
      runExists packetType $ \case
        Literal -> literalNumber <&> \l -> Packet packetVersion Literal l
        Operation t -> operationPayload <&> \ps -> Packet packetVersion (Operation t) ps

sumVersionNumbers :: Packet -> Int
sumVersionNumbers (Packet v t l) = v + case t of
  Literal -> 0
  Operation _ -> sum $ sumVersionNumbers <$> l

evaluatePacket :: Packet -> Int
evaluatePacket = \case
  (Packet _ Literal n) -> n
  (Packet _ (Operation op) subPackets) ->
    let subResults = evaluatePacket <$> subPackets
     in ($subResults) $ case op of
       Sum -> sum
       Product -> product
       Minimum -> minimum
       Maximum -> maximum
       Greater -> withTwo (\a b -> if a > b then 1 else 0)
       Less -> withTwo (\a b -> if a < b then 1 else 0)
       Equal -> withTwo (\a b -> if a == b then 1 else 0)
  where
    withTwo f [a,b] = f a b
    withTwo f _ = error "Comparison packets need to have exactly two sub packets"

solution1 :: IO ()
solution1 = do
  input <- hexStringToBinaryString <$> readFile "inputs/day16/input1"
  let packet = P.parse parser "packet" input
  print $ sumVersionNumbers <$> packet
  pure ()

solution2 :: IO ()
solution2 = do
  input <- hexStringToBinaryString <$> readFile "inputs/day16/input1"
  let packet = P.parse parser "packet" input
  print $ evaluatePacket <$> packet
  pure ()
