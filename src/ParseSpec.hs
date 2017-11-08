{-# LANGUAGE LambdaCase #-}
module ParseSpec (parseSpecFile) where

import qualified Data.ByteString as BS
import           Data.String (fromString)
import           Prelude hiding (sequence)
import           System.FilePath (takeBaseName)
import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language (emptyDef, haskellStyle)
import           Text.Parsec.String
import qualified Text.Parsec.Token as P

import           AST

parseSpecFile :: FilePath -> IO (Maybe Specification)
parseSpecFile filePath =
  parseFromFile (specification filePath) filePath >>= \case
    Left err -> do
      print err
      return Nothing
              
    Right spec -> 
      return $ Just spec

specification :: FilePath -> Parser Specification
specification filePath =
  let name = fromString $ takeBaseName filePath
  in do
    statements <- many1 statement
    eof
    return $ Specification filePath name statements

statement :: Parser Statement
statement = expectValue
        <|> expectEnum
        <|> expectConst
        <|> expectData
        <|> sequence
        <|> if_
        <|> byteOrder

expectValue :: Parser Statement
expectValue = do
  reserved "expect-value"
  type_  <- type_
  name   <- byteStringLiteral
  format <- format
  semicolon
  return $ StmtExpectValue $ Named name (type_, format)

expectEnum :: Parser Statement
expectEnum = do
  reserved "expect-enum"
  type_  <- type_
  name   <- byteStringLiteral
  format <- optionMaybe $ try format
  case format of
    Just (FormatEnum e) -> do
      semicolon
      return $ StmtExpectEnum (map fst e) $ Named name (type_, FormatEnum e)

    Just format -> do
      e <- enum
      semicolon
      return $ StmtExpectEnum e $ Named name (type_, format)

    Nothing -> do
      e <- enum
      semicolon
      return $ StmtExpectEnum e $ Named name (type_, FormatDefault)
  where
    enum = brackets (commaSep1 natural)

expectConst :: Parser Statement
expectConst = do
  reserved "expect-const"
  type_ <- type_
  value <- natural
  semicolon
  return $ StmtExpectConstant $ Constant type_ value

expectData :: Parser Statement
expectData = do
  reserved "expect-data"
  name <- byteStringLiteral
  reserved "of-length"
  length <- (natural >>= return . LengthConstant) <|> (byteStringLiteral >>= return . LengthVariable)
  semicolon
  return $ StmtExpectData $ Named name length

sequence :: Parser Statement
sequence = do
  reserved "sequence"
  name       <- byteStringLiteral
  statements <- braces $ many1 statement
  return $ StmtSequence $ Named name statements

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
byteOrder = swap >>= return . StmtByteOrder
  where
    swap = reserved "byte-order-swap" >> return ByteOrderSwap

expression :: Parser Expression
expression = buildExpressionParser exprTable term <?> "expression"
  where
    term     = parens expression <|> variable <|> constant <?> "simple expression"
    variable = byteStringLiteral >>= return . ExprVariable
    constant = integer >>= return . ExprConstant

format :: Parser Format
format = (reserved "hex" >> return FormatHex)
     <|> formatEnum
     <|> return FormatDefault
  where
    formatEnum = brackets (commaSep1 assignment) >>= return . FormatEnum
      where
        assignment = do
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
  , P.reservedNames = [ "expect-value", "expect-const", "expect-enum", "expect-data"
                      , "sequence", "of-length", "if", "else"
                      , "byte-order-swap"
                      , "uint8", "uint16", "uint32", "int8", "int16", "int32"
                      , "hex"
                      ]
  , P.reservedOpNames = [ "->", "&&", "||", "==", "!=", "+", "-", "*", "/" ]
}

exprTable = [ [prefix "-"  UnaryPlus, prefix "+" UnaryMinus ]
            , [binary "*"  BinTimes AssocLeft, binary "/"  BinDiv      AssocLeft]
            , [binary "+"  BinPlus  AssocLeft, binary "-"  BinMinus    AssocLeft]
            , [binary "==" BinEqual AssocLeft, binary "!=" BinNotEqual AssocLeft]
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
