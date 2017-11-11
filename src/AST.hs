module AST where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map

type Specifications = Map.Map BS.ByteString Specification

data Specification = Specification { filePath   :: FilePath
                                   , name       :: BS.ByteString
                                   , toplevel   :: Bool
                                   , statements :: [Statement]
                                   }

data Statement = StmtExpectConstant Type Value
               | StmtExpectValue    ExpectValue
               | StmtExpectEnum     [Int] BS.ByteString Type Format
               | StmtExpectData     BS.ByteString Length
               | StmtExpectAscii    BS.ByteString Length
               | StmtSequence       BS.ByteString [Statement]
               | StmtIf             Expression [Statement] (Maybe [Statement])
               | StmtByteOrder      ByteOrder
               | StmtLet            BS.ByteString Expression
               | StmtTry            [BS.ByteString]

data ExpectValue = EVSingle   BS.ByteString Type Format
                 | EVSequence BS.ByteString Type Length Format
                 | EVPacked   Assignment Type Format

data Expression = ExprConstant Int
                | ExprVariable BS.ByteString
                | ExprUnary UnaryOp Expression
                | ExprBinary BinaryOp Expression Expression

data BinaryOp = BinAnd  | BinOr        | BinEqual   | BinNotEqual 
              | BinPlus | BinMinus     | BinTimes   | BinDiv
              | BinLess | BinLessEqual | BinGreater | BinGreaterEqual

data UnaryOp = UnaryPlus | UnaryMinus

data Length = LengthConstant Int
            | LengthVariable BS.ByteString

data Format = FormatDefault
            | FormatHex
            | FormatEnum Assignment
            deriving Eq

type Assignment = [(Value, BS.ByteString)]

type Value = Int

data Type = TypeUInt8
          | TypeUInt16
          | TypeUInt32
          | TypeInt8
          | TypeInt16
          | TypeInt32

data ByteOrder = ByteOrderBigEndian
               | ByteOrderLittleEndian
               | ByteOrderSwap
