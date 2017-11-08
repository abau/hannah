module AST where

import qualified Data.ByteString as BS

data Type = TypeUInt8
          | TypeUInt16
          | TypeUInt32
          | TypeInt8
          | TypeInt16
          | TypeInt32

type Value = Int

data Constant = Constant Type Value

data Format = FormatDefault
            | FormatHex
            | FormatEnum [(Value, BS.ByteString)]

data Named a = Named BS.ByteString a

data Length = LengthConstant Int
            | LengthVariable BS.ByteString

data UnaryOp = UnaryPlus | UnaryMinus

data BinaryOp = BinAnd | BinOr | BinEqual | BinNotEqual 
              | BinPlus | BinMinus | BinTimes | BinDiv

data Expression = ExprConstant Int
                | ExprVariable BS.ByteString
                | ExprUnary UnaryOp Expression
                | ExprBinary BinaryOp Expression Expression

data ByteOrder = ByteOrderBigEndian
               | ByteOrderLittleEndian
               | ByteOrderSwap

data Statement = StmtExpectConstant Constant
               | StmtExpectValue (Named (Type, Format))
               | StmtExpectEnum [Int] (Named (Type, Format))
               | StmtExpectData (Named Length)
               | StmtSequence (Named [Statement])
               | StmtIf Expression [Statement] (Maybe [Statement])
               | StmtByteOrder ByteOrder

data Specification = Specification { filePath   :: FilePath
                                   , name       :: BS.ByteString
                                   , statements :: [Statement]
                                   }
