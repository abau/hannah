{-# LANGUAGE LambdaCase #-}
module Read (trySpecificationsOnFile) where

import           Control.Monad (when, guard, unless, forM, forM_)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), runMaybeT)
import           Control.Monad.Trans.State.Strict (StateT (StateT), runStateT, gets, modify')
import qualified Data.Binary.Get as B
import           Data.Bits ((.&.), shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import           Data.Word (Word8)
import           Prelude hiding (Read)
import qualified System.Endian as E
import qualified System.IO as IO
import           Text.Printf (printf)

import AST
import Util

trySpecificationsOnFile :: Specifications -> FilePath -> Bool -> Bool -> IO (Maybe Values)
trySpecificationsOnFile specs filePath printOutput debug =
  let toplevelSpecs = filter toplevel $ Map.elems specs
  in
    IO.withBinaryFile filePath IO.ReadMode $ \handle -> do
      result <- runMaybeT $ runStateT (trySpecifications toplevelSpecs) 
                          $ defaultEnv specs handle debug
      case result of
        Just (_, env) -> do
          when printOutput $ BS.putStr $ output env
          return $ Just $ values env
        Nothing -> return Nothing

trySpecifications :: [Specification] -> Read ()
trySpecifications = go 0
  where
    go i = \case
      []   -> failRead
      s:ss -> (withPrefix i $ readSpecification s) `catch` (go (i+1) ss)

readSpecification :: Specification -> Read ()
readSpecification specification = do
  printBlock (name specification) []
  mapEnv $ \env -> env { specFilePath = filePath specification }
  withIncreasedIndent $ readStatements $ statements specification

readStatements :: [Statement] -> Read ()
readStatements = mapM_ readStatement

readStatement :: Statement -> Read ()
readStatement = \case
  StmtExpectConstant t v   -> readConstant t v
  StmtExpectValue v        -> readValue v
  StmtExpectEnum e n t f a -> readEnum e n t f a
  StmtExpectData n l       -> readData n l
  StmtExpectAscii n l      -> readAscii n l
  StmtSequence n l s       -> readSequence n l s
  StmtIf c t f             -> readIf c t f
  StmtByteOrder b          -> readByteOrder b
  StmtLet n e              -> readLet n e
  StmtTry s                -> readTry s

readConstant :: Type -> Value -> Read ()
readConstant t v = readValueOfType t >>= guard . ((==) v)

readValue :: ExpectValue -> Read ()
readValue = \case
  EVSingle name type_ format assignment -> do
    value <- readValueOfType type_
    printValues name type_ format assignment [value]
    addValue name value

  EVSequence name type_ length format assignment -> do
    length <- readLength length
    values <- forM [0..length - 1] $ \i -> do
      value <- readValueOfType type_
      withPrefix i $ addValue name value
      return value
    printValues name type_ format assignment values

  EVPacked assignment type_ format -> do
    value <- readValueOfType type_
    go value $ reverse assignment
    where
      go value = \case
        [] -> return ()
        (numBits, name):assignment ->
          let effective = value .&. ((2 ^ numBits) - 1)
              value'    = value `shiftR` numBits
          in do
            go value' assignment
            printValues name (requiredType numBits) format Nothing [effective]
            addValue name effective

readEnum :: [Int] -> BS.ByteString -> Type -> Format -> Maybe Assignment -> Read ()
readEnum enum name type_ format assignment = do
  value <- readValueOfType type_
  guard $ value `elem` enum
  printValues name type_ format assignment [value]
  addValue name value

readData :: BS.ByteString -> Length -> Read ()
readData name length = do
  length <- readLength length
  chunk  <- readChunk  length
  forM_ [0..length - 1] $ \i ->
    let value = fromIntegral $ BS.index chunk i
    in
      withPrefix i $ addValue name value
  printData chunk
  where
    printData data_ = 
      let hexLines  = makeHexLines data_
          charLines = makeCharLines data_
      in
        printBlock name $ mix hexLines charLines

    makeHexLines = makeLines $ toLength 23 . map8
      where
        map8 = BS.intercalate (fromString " ") 
             . map (fromString . printf "%02x" . fromEnum)
             . BS.unpack

    makeCharLines = makeLines $ toLength 8 . map8
      where
        map8        = BS.map $ \c -> if printable c then c else c2w '.'
        printable w = Char.isAscii (w2c w) && Char.isPrint (w2c w)

    toLength n string = BS.append string $ space $ n - BS.length string

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

readAscii :: BS.ByteString -> Length -> Read ()
readAscii name length = do
  length <- readLength length
  chunk  <- readChunk  length
  unless (isAscii chunk) failRead
  forM_ [0..length - 1] $ \i ->
    let value = fromIntegral $ BS.index chunk i
    in
      withPrefix i $ addValue name value
  printData chunk
  where
    isAscii     = all (Char.isAscii . w2c) . BS.unpack
    printData   = printBlock name . map (BS.filter isPrintable) . BS.splitWith isNewline
    isNewline   = (==) '\n' . w2c
    isPrintable = Char.isPrint . w2c

readSequence :: BS.ByteString -> Maybe Length -> [Statement] -> Read ()
readSequence name length statements = mapM readLength length >>= go 0
  where
    go i = \case
      Nothing -> do
        isEOF <- fromEnv handle >>= runIO . IO.hIsEOF
        unless isEOF $ do
          printBlock name []
          withPrefix i $ withIncreasedIndent $ readStatements statements
          go (i + 1) Nothing
    
      Just length | i < length -> do
        printBlock name []
        withPrefix i $ withIncreasedIndent $ readStatements statements
        go (i + 1) $ Just length

      Just _ -> return ()

readIf :: Expression -> [Statement] -> Maybe [Statement] -> Read ()
readIf condition true mFalse = do
  c <- readExpression condition
  if c > 0 then withPrefix 0 $ readStatements true
           else case mFalse of
                  Nothing    -> return ()
                  Just false -> withPrefix 1 $ readStatements false

readByteOrder :: ByteOrder -> Read ()
readByteOrder = \case
  ByteOrderBigEndian    -> mapEnv $ \env -> env { byteOrder = E.BigEndian }
  ByteOrderLittleEndian -> mapEnv $ \env -> env { byteOrder = E.LittleEndian }
  ByteOrderSwap         -> mapEnv $ \env -> 
                             case byteOrder env of
                               E.BigEndian    -> env { byteOrder = E.LittleEndian }
                               E.LittleEndian -> env { byteOrder = E.BigEndian }

readLet :: BS.ByteString -> Expression -> Read ()
readLet name expr = do
  value <- readExpression expr
  addValue name value

readTry :: [BS.ByteString] -> Read ()
readTry names = do
  specs <- forM names $ \n ->
             fromEnv specs >>= return . Map.lookup n >>= \case
               Just s  -> return s
               Nothing -> failSpec $ "Could not find specification " ++ (show n)
  trySpecifications specs

readLength :: Length -> Read Int
readLength = \case
 LengthConstant c -> return c
 LengthVariable v -> valueFromEnv v

readExpression :: Expression -> Read Int
readExpression e = do
  prefix <- fromEnv prefix
  value  <- fromEnv values >>= return . evaluate e prefix
  case value of
    Nothing -> failSpec "Could not evaluate expression"
    Just v  -> return v

readValueOfType :: Type -> Read Value
readValueOfType = \case
  TypeUInt8    -> readNum 1 B.getWord8    B.getWord8
  TypeUInt16   -> readNum 2 B.getWord16le B.getWord16be
  TypeUInt32   -> readNum 4 B.getWord32le B.getWord32be
  TypeInt8     -> readNum 1 B.getInt8     B.getInt8
  TypeInt16    -> readNum 2 B.getInt16le  B.getInt16be
  TypeInt32    -> readNum 4 B.getInt32le  B.getInt32be

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
addValue name value = do
  prefix <- fromEnv prefix
  mapEnv $ \env -> env { values = Map.insert (name, prefix) value $ values env }

printValues :: BS.ByteString -> Type -> Format -> Maybe Assignment -> [Value] -> Read ()
printValues label type_ format assignment values = do
  indent <- fromEnv indent
  toOutput [space indent, label, fromString ": "]
  let values' = map (\v -> formatValue v type_ format assignment) values
  toOutput [BS.intercalate (space 1) values', newline]

printBlock :: BS.ByteString -> [BS.ByteString] -> Read ()
printBlock label lines = do
  indent <- fromEnv indent
  toOutput [space indent, label, fromString ": "]
  case lines of
    []   -> toOutput [newline]
    l:ls -> do toOutput [l, newline]
               toOutput $ concat $ do
                 let indent' = indent + (BS.length label) + 2
                 l <- ls
                 return [space indent', l, newline]

toOutput :: [BS.ByteString] -> Read ()
toOutput strings = do
  debug <- fromEnv debug
  when debug $ forM_ strings $ runIO . BS.putStr
  mapEnv $ \env -> env { output = BS.concat $ (output env) : strings }

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

withPrefix :: Int -> Read a -> Read a
withPrefix p run = do
  mapEnv $ \env -> env { prefix = p : (prefix env) }
  result <- run
  mapEnv $ \env -> env { prefix = tail $ prefix env }
  return result

mapEnv :: (Env -> Env) -> Read ()
mapEnv = modify'

valueFromEnv :: BS.ByteString -> Read Value
valueFromEnv name = do
  prefix <- fromEnv prefix
  fromEnv values >>= return . getValue name prefix >>= \case
    Just value -> return value
    Nothing    -> failSpec $ "Could not find variable " ++ (show name)

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

defaultEnv :: Specifications -> IO.Handle -> Bool -> Env
defaultEnv specs handle = Env specs handle 0 Map.empty BS.empty "" E.getSystemEndianness []

type Read a = StateT Env (MaybeT IO) a

data Env = Env { specs        :: Specifications
               , handle       :: IO.Handle
               , indent       :: Int
               , values       :: Values
               , output       :: BS.ByteString
               , specFilePath :: FilePath
               , byteOrder    :: E.Endianness
               , prefix       :: [Int]
               , debug        :: Bool
               }
