{-# LANGUAGE LambdaCase #-}
module ParseSpec (parseSpecFile) where

import qualified Data.ByteString as BS
import           Data.String (fromString)
import           Prelude hiding (sequence, length)
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language (emptyDef, haskellStyle)
import           Text.Parsec.String
import qualified Text.Parsec.Token as P

import           AST hiding (statements)

parseSpecFile :: FilePath -> IO (Maybe [Statement])
parseSpecFile filePath =
  parseFromFile statements filePath >>= \case
    Left err -> do
      print err
      return Nothing
              
    Right spec -> 
      return $ Just spec

statements :: Parser [Statement]
statements = do
  statements <- many1 statement
  eof
  return statements

statement :: Parser Statement
statement = expectValue
        <|> expectEnum
        <|> expectConst
        <|> expectData
        <|> expectAscii
        <|> sequence
        <|> if_
        <|> byteOrder
        <|> let_
        <|> try_

expectValue :: Parser Statement
expectValue = do
  reserved "expect-value"
  unpacked <|> packed
  where
    unpacked = do
      name   <- byteStringLiteral
      type_  <- type_
      choice [ do format <- format
                  assign <- optionMaybe assignment
                  semicolon
                  return $ StmtExpectValue $ EVSingle name type_ format assign
             
             , do length <- length
                  format <- format
                  assign <- optionMaybe assignment
                  semicolon
                  return $ StmtExpectValue $ EVSequence name type_ length format assign
             ]
    packed = do
      assignment <- assignment
      type_  <- type_
      format <- format
      semicolon
      return $ StmtExpectValue $ EVPacked assignment type_ format

expectEnum :: Parser Statement
expectEnum = do
  reserved "expect-enum"
  name   <- byteStringLiteral
  type_  <- type_
  format <- format
  assign <- optionMaybe $ try assignment
  case assign of
    Just a -> do
      semicolon
      return $ StmtExpectEnum (map fst a) name type_ format $ Just a

    Nothing -> do
      e <- brackets (commaSep1 natural)
      semicolon
      return $ StmtExpectEnum e name type_ format Nothing

expectConst :: Parser Statement
expectConst = do
  reserved "expect-const"
  type_ <- type_
  value <- natural
  semicolon
  return $ StmtExpectConstant type_ value

expectData :: Parser Statement
expectData = do
  reserved "expect-data"
  name <- byteStringLiteral
  length <- length
  semicolon
  return $ StmtExpectData name length

expectAscii :: Parser Statement
expectAscii = do
  reserved "expect-ascii"
  name <- byteStringLiteral
  length <- length
  semicolon
  return $ StmtExpectAscii name length

sequence :: Parser Statement
sequence = do
  reserved "sequence"
  name       <- byteStringLiteral
  length     <- optionMaybe length
  statements <- braces $ many1 statement
  return $ StmtSequence name length statements

if_ :: Parser Statement
if_ = do
  reserved "if"
  condition <- parens expression
  true      <- braces $ many1 statement
  false     <- optionMaybe $ do
                 reserved "else"
                 choice [braces (many1 statement), if_ >>= return . return]
  return $ StmtIf condition true false

byteOrder :: Parser Statement
byteOrder = do
  byteorder <- little <|> big <|> swap
  semicolon
  return $ StmtByteOrder byteorder
  where
    little = reserved "byte-order-little-endian" >> return ByteOrderLittleEndian
    big    = reserved "byte-order-big-endian"    >> return ByteOrderBigEndian
    swap   = do
      reserved "byte-order-system"
      condition <- parens expression
      return $ ByteOrderSystem condition

let_ :: Parser Statement
let_ = do
  reserved "let"
  name <- byteStringLiteral
  reservedOp "="
  expr <- expression
  semicolon
  return $ StmtLet name expr

try_ :: Parser Statement
try_ = do
  reserved "try"
  names <- brackets (commaSep1 byteStringLiteral)
  semicolon
  return $ StmtTry names

length :: Parser Length
length = do
  reserved "of-length"
  (natural >>= return . LengthConstant) <|> (byteStringLiteral >>= return . LengthVariable)

expression :: Parser Expression
expression = buildExpressionParser exprTable term <?> "expression"
  where
    term     = parens expression <|> variable <|> constant <?> "simple expression"
    variable = byteStringLiteral >>= return . ExprVariable
    constant = integer >>= return . ExprConstant

format :: Parser Format
format = (reserved "hex"  >> return FormatHex)
     <|> return FormatDec

assignment :: Parser Assignment
assignment = brackets $ commaSep1 assign
  where
    assign = do
      i <- natural
      reservedOp "->"
      s <- byteStringLiteral
      return (i, s)

type_ :: Parser Type
type_ = (reserved "uint8"  >> return TypeUInt8)
    <|> (reserved "uint16" >> return TypeUInt16)
    <|> (reserved "uint32" >> return TypeUInt32)
    <|> (reserved "int8"   >> return TypeInt8)
    <|> (reserved "int16"  >> return TypeInt16)
    <|> (reserved "int32"  >> return TypeInt32)

language = emptyDef {
    P.commentLine = "#"
  , P.opStart = P.opStart haskellStyle
  , P.opLetter = P.opLetter haskellStyle
  , P.reservedNames = [ "expect-value", "expect-const", "expect-enum", "expect-data", "expect-ascii"
                      , "sequence", "of-length", "if", "else", "let", "try"
                      , "byte-order-system", "byte-order-little-endian", "byte-order-big-endian"
                      , "uint8", "uint16", "uint32", "int8", "int16", "int32"
                      , "hex"
                      ]
  , P.reservedOpNames = [ "->", "&&", "||", "==", "!=", "+", "-", "*", "/", "="
                        , ">", ">=", "<", "<="
                        ]
}

exprTable = [ [prefix "-"  UnaryPlus, prefix "+" UnaryMinus ]
            , [binary "*"  BinTimes AssocLeft, binary "/"  BinDiv   AssocLeft]
            , [binary "+"  BinPlus  AssocLeft, binary "-"  BinMinus AssocLeft]
            , [ binary "<"  BinLess    AssocLeft, binary "<=" BinLessEqual AssocLeft
              , binary ">"  BinGreater AssocLeft, binary ">=" BinGreaterEqual AssocLeft
              ]
            , [binary "==" BinEqual AssocLeft, binary "!=" BinNotEqual  AssocLeft]
            , [binary "&&" BinAnd   AssocLeft]
            , [binary "||" BinOr    AssocLeft]
            ]
  where
    binary name op = Infix  $ reservedOp name >> return (ExprBinary op)
    prefix name op = Prefix $ reservedOp name >> return (ExprUnary  op)

lexer             = P.makeTokenParser language
identifier        = P.identifier lexer
reserved          = P.reserved lexer
reservedOp        = P.reservedOp lexer
byteStringLiteral = stringLiteral >>= return . fromString
stringLiteral     = P.stringLiteral lexer
integer           = P.integer lexer >>= return . fromIntegral
natural           = P.natural lexer >>= return . fromIntegral
brackets          = P.brackets lexer
braces            = P.braces lexer
parens            = P.parens lexer
semicolon         = P.semi lexer
commaSep1         = P.commaSep1 lexer
semiSep1          = P.semiSep1 lexer
