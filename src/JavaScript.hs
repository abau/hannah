{-# LANGUAGE LambdaCase #-}
module JavaScript (writeJavaScript) where

import           Control.Monad (unless, when, forM_)
import qualified Data.ByteString as BS
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import           Prelude hiding (print)
import qualified System.IO as IO

import AST
import Util

writeJavaScript :: Specifications -> FilePath -> IO ()
writeJavaScript specs filePath = IO.withFile filePath IO.ReadMode go
  where
    go handle = do
      eof <- IO.hIsEOF handle
      unless eof $ do
        line <- IO.hGetLine handle
        putStrLn line
        when ("INSERT HANNAH SPECIFICATION NAMES HERE" `List.isInfixOf` line) $ do
          print "<ul style=\"font-family:monospace;\">"
          forM_ leafSpecNames $ \name -> print "<li>" >> BS.putStr name >> print "</li>"
          print "</ul>"

        when ("INSERT HANNAH SPECIFICATIONS HERE" `List.isInfixOf` line) $ do
          forM_ (Map.elems specs) writeSpecification
          writeStatement $ StmtTry $ map name toplevelSpecs
        go handle

    toplevelSpecs = filter toplevel $ Map.elems specs
    leafSpecNames = List.filter (\n -> all (\m -> not $ strictInfix n m) names) names
      where
        strictInfix n m = (n /= m) && (BS.isInfixOf n m)
        names           = map name $ Map.elems specs

writeSpecification :: Specification -> WriteJS ()
writeSpecification spec = do
  print $ concat ["function read_", fromSpecificationName $ name spec, "(){"]
  print $ concat ["specification = ", show $ name spec, ";"]
  print $ concat ["if (position === 0) { output.data =", show $ name spec, ";}"]
  forM_ (statements spec) writeStatement
  print "}"

writeStatement :: Statement -> WriteJS ()
writeStatement = \case
  StmtExpectConstant t v ->
    writeCall "readConstant" [writeType t, writeValue v]
  StmtExpectValue (EVSingle n t f a) ->
    writeCall "readValue" [writeName n, writeType t, writeFormat f, writeAssignment a]
  StmtExpectValue (EVSequence n t l f a) ->
    writeCall "readValueSequence" [ writeName n, writeType t, writeLength l
                                  , writeFormat f, writeAssignment a ]
  StmtExpectValue (EVPacked a t f) ->
    writeCall "readValuePacked" [writeAssignment $ Just a, writeType t, writeFormat f]
  StmtExpectEnum e n t f a ->
    writeCall "readEnum" [writeEnum e, writeName n, writeType t, writeFormat f, writeAssignment a]
  StmtExpectData n l ->
    writeCall "readData" [writeName n, writeLength l]
  StmtExpectAscii n l ->
    writeCall "readAscii" [writeName n, writeLength l]
  StmtSequence n l s ->
    writeCall "readSequence" [writeName n, writeSequenceLength l, writeStatements s]
  StmtIf c t f ->
    writeCall "readIf" [ writeExpression c, writeStatements t
                       , writeStatements $ Maybe.fromMaybe [] f ]
  StmtByteOrder ByteOrderBigEndian ->
    writeCall "readByteOrder" [print "ByteOrder.BigEndian"]
  StmtByteOrder ByteOrderLittleEndian ->
    writeCall "readByteOrder" [print "ByteOrder.LittleEndian"]
  StmtByteOrder (ByteOrderSystem c) ->
    writeCall "readByteOrderSystem" [writeExpression c]
  StmtLet n e ->
    writeCall "readLet" [writeName n, writeExpression e]
  StmtTry s ->
    writeCall "trySpecifications" [ writeSpecifications s ]

writeSpecifications :: [BS.ByteString] -> WriteJS ()
writeSpecifications specs = do
  print "["
  sequence $ List.intersperse (print ",") $ flip map specs $ \s -> do
    print "function (){"
    writeCall ("read_" ++ fromSpecificationName s) []
    print "}"
  print "]"

writeSequenceLength :: SequenceLength -> WriteJS ()
writeSequenceLength = \case
  SeqLengthEOF             -> print "undefined"
  SeqLengthFixed l         -> writeLength l
  SeqLengthPostCondition c -> do
    print "function (){return "
    writeExpression c
    print ";}"

writeExpression :: Expression -> WriteJS ()
writeExpression = \case
  ExprConstant c      -> writeValue c
  ExprVariable v      -> print "getValueFromEnv (" >> writeName v
                                                   >> print ")"
  ExprUnary op e1     -> print "(" >> writeUnary op
                                   >> writeExpression e1
                                   >> print ")"
  ExprBinary op e1 e2 -> print "(" >> writeExpression e1
                                   >> writeBinary op
                                   >> writeExpression e2
                                   >> print ")"
  ExprFilePosition    -> print "position"
  where
    writeUnary  UnaryPlus       = print "+"
    writeUnary  UnaryMinus      = print "-"
    writeBinary BinAnd          = print "&&"
    writeBinary BinOr           = print "||"
    writeBinary BinEqual        = print "==="
    writeBinary BinNotEqual     = print "!=="
    writeBinary BinPlus         = print "+"
    writeBinary BinMinus        = print "-"
    writeBinary BinTimes        = print "*"
    writeBinary BinDiv          = print "/"
    writeBinary BinLess         = print "<"
    writeBinary BinLessEqual    = print "<="
    writeBinary BinGreater      = print ">"
    writeBinary BinGreaterEqual = print ">="

writeStatements :: [Statement] -> WriteJS ()
writeStatements statements = do
  print "function (){"
  forM_ statements writeStatement
  print "}"

writeAssignment :: Maybe Assignment -> WriteJS ()
writeAssignment = \case
  Nothing -> print "undefined"
  Just a  -> do
    print "["
    sequence $ List.intersperse (print ",") $ flip map a $ \(v,n) -> do
      print "["
      writeValue v
      print ","
      writeName n
      print "]"
    print "]"

writeType :: Type -> WriteJS ()
writeType = \case
  TypeUInt8  -> print "Type.UInt8"
  TypeUInt16 -> print "Type.UInt16"
  TypeUInt32 -> print "Type.UInt32"
  TypeInt8   -> print "Type.Int8"
  TypeInt16  -> print "Type.Int16"
  TypeInt32  -> print "Type.Int32"

writeValue :: Value -> WriteJS ()
writeValue = print . show

writeFormat :: Format -> WriteJS ()
writeFormat = \case
  FormatDec -> print "Format.Dec"
  FormatHex -> print "Format.Hex"

writeLength :: Length -> WriteJS ()
writeLength = \case
  LengthConstant c -> print $ show $ show c
  LengthVariable v -> print $ show v

writeEnum :: [Int] -> WriteJS ()
writeEnum = print . show

writeCall :: String -> [WriteJS ()] -> WriteJS ()
writeCall name args = do
  print $ concat [name, "("]
  sequence $ List.intersperse (print ",") args
  print ");"

writeName :: BS.ByteString -> WriteJS ()
writeName = print . show

fromSpecificationName :: BS.ByteString -> String
fromSpecificationName = map (go . w2c) . BS.unpack
  where
    go '/' = '_'
    go c   = c

print :: String -> WriteJS ()
print = putStr

type WriteJS = IO
