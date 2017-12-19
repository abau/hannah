{-# LANGUAGE LambdaCase #-}
module Util where

import           Control.Monad (ap)
import qualified Data.ByteString as BS
import           Data.Int
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import           Data.Word
import           Text.Printf (printf)

import AST

type Prefix   = [Int]
type Values   = Map.Map (BS.ByteString, Prefix) Value

getValue :: BS.ByteString -> Prefix -> Values -> Maybe Value
getValue name prefix values = go prefix
  where
    go prefix = case Map.lookup (name, prefix) values of
      Just v  -> return v
      Nothing -> case prefix of
        []   -> Nothing
        _:ss -> getValue name ss values

toPrefixName :: BS.ByteString -> [Int] -> BS.ByteString
toPrefixName n = BS.append n . foldr append BS.empty
  where
    append i = BS.append $ BS.concat [fromString "-", fromString $ show i]

formatValue :: Value -> Type -> Format -> Maybe Assignment -> BS.ByteString
formatValue value type_ format = \case
  Nothing -> print value
  Just a  -> case lookup value a of
    Nothing -> print value
    Just s  -> s
  where
    print v = case format of
      FormatDec -> fromString $ printf "%d" v
      FormatHex -> case type_ of
        TypeUInt8  -> fromString $ printf "0x%02x" v
        TypeInt8   -> fromString $ printf "0x%02x" v
        TypeUInt16 -> fromString $ printf "0x%04x" v
        TypeInt16  -> fromString $ printf "0x%04x" v
        TypeUInt32 -> fromString $ printf "0x%08x" v
        TypeInt32  -> fromString $ printf "0x%08x" v

requiredType :: Int -> Type
requiredType = \case
  i | i <= 8  -> TypeUInt8
  i | i <= 16 -> TypeUInt16
  i | i <= 32 -> TypeUInt32
  i           -> error $ "requiredType undefined on " ++ (show i)

bounds :: Either Int Type -> (Value, Value)
bounds = \case
  Left  i          -> (0, (2^i) - 1)
  Right TypeUInt8  -> (fromIntegral (minBound :: Word8) , fromIntegral (maxBound :: Word8))
  Right TypeUInt16 -> (fromIntegral (minBound :: Word16), fromIntegral (maxBound :: Word16))
  Right TypeUInt32 -> (fromIntegral (minBound :: Word32), fromIntegral (maxBound :: Word32))
  Right TypeInt8   -> (fromIntegral (minBound :: Int8),   fromIntegral (maxBound :: Int8))
  Right TypeInt16  -> (fromIntegral (minBound :: Int16),  fromIntegral (maxBound :: Int16))
  Right TypeInt32  -> (fromIntegral (minBound :: Int32),  fromIntegral (maxBound :: Int32))

evaluate :: Expression -> Prefix -> Value -> Values -> Maybe Value
evaluate expression prefix filePos env = case expression of
  ExprConstant i      -> return i
  ExprVariable v      -> getValue v prefix env
  ExprUnary op e1     -> return (unary  op) `ap` (evaluate e1 prefix filePos env)
  ExprBinary op e1 e2 -> return (binary op) `ap` (evaluate e1 prefix filePos env)
                                            `ap` (evaluate e2 prefix filePos env)
  ExprFilePosition    -> return filePos
  where
    unary UnaryPlus  v = v
    unary UnaryMinus v = -v

    binary BinAnd          v1 v2 = b2i $ (i2b v1) && (i2b v2)
    binary BinOr           v1 v2 = b2i $ (i2b v1) || (i2b v2)
    binary BinEqual        v1 v2 = b2i $ v1 == v2
    binary BinNotEqual     v1 v2 = b2i $ v1 /= v2
    binary BinPlus         v1 v2 = v1 + v2
    binary BinMinus        v1 v2 = v1 - v2
    binary BinTimes        v1 v2 = v1 * v2
    binary BinDiv          v1 v2 = v1 `div` v2
    binary BinLess         v1 v2 = b2i $ v1 < v2
    binary BinLessEqual    v1 v2 = b2i $ v1 <= v2
    binary BinGreater      v1 v2 = b2i $ v1 > v2
    binary BinGreaterEqual v1 v2 = b2i $ v1 >= v2

    i2b 0     = False
    i2b _     = True
    b2i False = 0
    b2i True  = 1

newline :: BS.ByteString
newline = BS.singleton $ c2w '\n'

space :: Int -> BS.ByteString
space i = BS.replicate i $ c2w ' '

c2w :: Char -> Word8
c2w = toEnum . fromEnum

w2c :: Word8 -> Char
w2c = toEnum . fromEnum
