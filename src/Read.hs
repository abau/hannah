{-# LANGUAGE LambdaCase #-}
module Read (trySpecificationsOnFile) where

import           Control.Monad (when, guard, unless, forM_, ap)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State.Strict (StateT (StateT), runStateT, gets, modify')
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Binary.Get as B
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import           Data.Word (Word8)
import           Prelude hiding (Read, print)
import qualified System.Endian as E
import qualified System.IO as IO
import           Text.Printf (printf)

import AST

trySpecificationsOnFile :: [Specification] -> FilePath -> IO Bool
trySpecificationsOnFile specs filePath =
  IO.withBinaryFile filePath IO.ReadMode $ \handle -> do
    result <- runMaybeT $ runStateT (trySpecifications specs) $ defaultEnv handle
    case result of
      Just (_, env) -> do
        BS.putStr $ output env
        return True
      Nothing -> return False

trySpecifications :: [Specification] -> Read ()
trySpecifications = \case
  []   -> failRead
  [s]  -> readSpecification s
  s:ss -> (readSpecification s) `catch` (trySpecifications ss)

readSpecification :: Specification -> Read ()
readSpecification specification = do
  printLn (name specification) []
  mapEnv $ \env -> env { specFilePath = filePath specification }
  withIncreasedIndent $ readStatements $ statements specification

readStatements :: [Statement] -> Read ()
readStatements = mapM_ readStatement

readStatement :: Statement -> Read ()
readStatement = \case
  StmtExpectConstant c  -> readConstant c
  StmtExpectValue v     -> readNamedValue v
  StmtExpectEnum enum v -> readEnum enum v
  StmtExpectData d      -> readNamedData d
  StmtSequence s        -> readSequence s
  StmtIf c t f          -> readIf c t f

readEnum :: [Int] -> Named (Type, Format) -> Read ()
readEnum enum (Named name (type_, format)) = do
  value <- readValue type_
  guard $ value `elem` enum
  printValue name format value
  addValue name value

readNamedValue :: Named (Type, Format) -> Read ()
readNamedValue (Named name (type_, format)) = do
  value <- readValue type_
  printValue name format value
  addValue name value

readConstant :: Constant -> Read ()
readConstant = \case
  Constant t i -> readValue t >>= guard . ((==) i)

readNamedData :: Named Length -> Read ()
readNamedData (Named name length) = case length of
  LengthConstant i -> readChunk i >>= printData
  LengthVariable v -> valueFromEnv v >>= readChunk >>= printData
  where
    printData data_ = 
      let hexLines  = makeHexLines data_
          charLines = makeCharLines data_
      in
        printLn name $ mix hexLines charLines

    makeHexLines = makeLines $ toLength 23 . map8
      where
        map8 = BS.intercalate (fromString " ") 
             . map (fromString . printf "%02x" . fromEnum)
             . BS.unpack

    makeCharLines = makeLines $ toLength 8 . map8
      where
        map8        = BS.map $ \c -> if printable c then c else c2w '.'
        printable c = Char.isAscii c' && Char.isPrint c'
          where
            c' = toEnum $ fromEnum c

    toLength n string = BS.append string $ BS.replicate (n - BS.length string) $ c2w ' '

    makeLines :: (BS.ByteString -> BS.ByteString) -> BS.ByteString -> [BS.ByteString]
    makeLines map8 data_ = 
      let (data16, rest) = BS.splitAt 16 data_
      in
        if BS.null data16 then []
        else 
          let (dataL8, dataH8) = BS.splitAt 8 data16
          in
            BS.concat [map8 dataL8, fromString "  ", map8 dataH8] : (makeLines map8 rest)

    mix = zipWith $ \x y -> BS.concat [x, fromString "    ", y]

readSequence :: Named [Statement] -> Read ()
readSequence (Named name statements) = go
  where
    go = do
      printLn name []
      withIncreasedIndent $ readStatements statements
      isEOF <- fromEnv handle >>= runIO . IO.hIsEOF
      unless isEOF go

readIf :: Expression -> [Statement] -> Maybe [Statement] -> Read ()
readIf condition true mFalse = do
  c <- readExpression condition
  if c > 0 then readStatements true
           else case mFalse of
                  Nothing    -> return ()
                  Just false -> readStatements false

readByteOrder :: ByteOrder -> Read ()
readByteOrder = \case
  ByteOrderBigEndian    -> mapEnv $ \env -> env { byteOrder = E.BigEndian }
  ByteOrderLittleEndian -> mapEnv $ \env -> env { byteOrder = E.LittleEndian }
  ByteOrderSwap         -> mapEnv $ \env -> 
                             case byteOrder env of
                               E.BigEndian    -> env { byteOrder = E.LittleEndian }
                               E.LittleEndian -> env { byteOrder = E.BigEndian }

readExpression :: Expression -> Read Int
readExpression = \case
  ExprConstant i      -> return i
  ExprVariable v      -> valueFromEnv v
  ExprUnary op e1     -> return (unary  op) `ap` (readExpression e1)
  ExprBinary op e1 e2 -> return (binary op) `ap` (readExpression e1) `ap` (readExpression e2)
  where
    unary UnaryPlus  v = v
    unary UnaryMinus v = -v

    binary BinAnd      v1 v2 = b2i $ (i2b v1) && (i2b v2)
    binary BinOr       v1 v2 = b2i $ (i2b v1) || (i2b v2)
    binary BinEqual    v1 v2 = b2i $ v1 == v2
    binary BinNotEqual v1 v2 = b2i $ v1 /= v2
    binary BinPlus     v1 v2 = v1 + v2
    binary BinMinus    v1 v2 = v1 - v2
    binary BinTimes    v1 v2 = v1 * v2
    binary BinDiv      v1 v2 = v1 `div` v2

    i2b 0     = False
    i2b _     = True
    b2i False = 0
    b2i True  = 1

readValue :: Type -> Read Value
readValue = \case
  TypeUInt8    -> readUInt8
  TypeUInt16   -> readUInt16
  TypeUInt32   -> readUInt32
  TypeInt8     -> readInt8
  TypeInt16    -> readInt16
  TypeInt32    -> readInt32

readInt32 :: Read Value
readInt32 = readNum 4 B.getInt32le B.getInt32be

readInt16 :: Read Value
readInt16 = readNum 2 B.getInt16le B.getInt16be

readInt8 :: Read Value
readInt8 = readNum 1 B.getInt8 B.getInt8

readUInt32 :: Read Value
readUInt32 = readNum 4 B.getWord32le B.getWord32be

readUInt16 :: Read Value
readUInt16 = readNum 2 B.getWord16le B.getWord16be

readUInt8 :: Read Value
readUInt8 = readNum 1 B.getWord8 B.getWord8

readNum :: Integral a => Int -> B.Get a -> B.Get a -> Read Value
readNum numBytes littleEndian bigEndian = do
  chunk     <- readChunk numBytes >>= return . LazyBS.fromStrict
  byteOrder <- fromEnv byteOrder
  return $ fromIntegral $ case byteOrder of
    E.LittleEndian -> B.runGet littleEndian chunk
    E.BigEndian    -> B.runGet bigEndian    chunk

readChunk :: Int -> Read BS.ByteString
readChunk n = do
  handle <- fromEnv handle
  chunk <- runIO $ BS.hGet handle n
  when (fromIntegral (BS.length chunk) < n) failRead
  return chunk

addValue :: BS.ByteString -> Value -> Read ()
addValue name value = mapEnv $ \env -> env { values = Map.insert name value $ values env }

printValue :: BS.ByteString -> Format -> Value -> Read ()
printValue label format value = do
  indent <- fromEnv indent
  print $ BS.replicate indent $ c2w ' '
  print label
  print $ fromString ": "
  case format of
    FormatDefault -> print $ fromString $ printf "%d" value
    FormatHex -> print $ fromString $ printf "%x" value
    FormatEnum e -> case lookup value e of
      Nothing -> print $ fromString $ printf "%d" value
      Just s -> print s
  print $ BS.singleton $ c2w '\n'

printLn :: BS.ByteString -> [BS.ByteString] -> Read ()
printLn label lines = do
  indent <- fromEnv indent
  print $ BS.replicate indent $ c2w ' '
  print label
  print $ fromString ": "
  case lines of
    []   -> printNewline
    l:ls -> do printWithNewline l 
               forM_ ls $ printLine $ indent + (BS.length label) + 2
  where
    printLine indent line   = print (BS.replicate indent $ c2w ' ') >> printWithNewline line
    printWithNewline string = print string >> printNewline
    printNewline            = print $ BS.singleton $ c2w '\n'

print :: BS.ByteString -> Read ()
print string =
  mapEnv $ \env -> env { output = BS.append (output env) string }

catch :: Read a -> Read a -> Read a
catch try handler = StateT $ \env -> MaybeT $ do
  pos <- IO.hTell $ handle env
  result <- runMaybeT $ runStateT try env
  case result of
    Just r -> return result
    Nothing -> do
      IO.hSeek (handle env) IO.AbsoluteSeek pos
      runMaybeT $ runStateT handler env

withIncreasedIndent :: Read a -> Read a
withIncreasedIndent read = do
  indent <- gets indent
  mapEnv $ \env -> env { indent = indent + 4 }
  result <- read
  mapEnv $ \env -> env { indent = indent }
  return result

mapEnv :: (Env -> Env) -> Read ()
mapEnv = modify'

valueFromEnv :: BS.ByteString -> Read Value
valueFromEnv name =
  fromEnv values >>= return . Map.lookup name >>= \case
    Just value -> return value
    Nothing    -> failSpec $ "Could not find variable '" ++ (show name) ++ "'"

failRead :: Read ()
failRead = lift $ fail ""

failSpec :: String -> Read a
failSpec msg = do
  specFilePath <- fromEnv specFilePath
  runIO $ error $ concat [specFilePath, ": ", msg]

runIO :: IO a -> Read a
runIO = lift . lift

fromEnv :: (Env -> a) -> Read a
fromEnv = gets

defaultEnv :: IO.Handle -> Env
defaultEnv handle = Env handle 0 Map.empty BS.empty "" E.getSystemEndianness

type Read a = StateT Env (MaybeT IO) a

data Env = Env { handle       :: IO.Handle
               , indent       :: Int
               , values       :: Map.Map BS.ByteString Value
               , output       :: BS.ByteString
               , specFilePath :: FilePath
               , byteOrder    :: E.Endianness
               }

c2w :: Char -> Word8
c2w = toEnum . fromEnum
