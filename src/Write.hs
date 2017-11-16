{-# LANGUAGE LambdaCase #-}
module Write (writeFile) where

import           Control.Monad (when, forM, forM_, foldM)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State.Strict (StateT (StateT), evalStateT, gets, modify')
import qualified Data.Binary.Put as B
import           Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LazyBS
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe, isJust, fromJust)
import           Data.String (fromString)
import           Prelude hiding (print, writeFile)
import qualified System.Endian as E
import qualified System.IO as IO
import           Text.Read (readMaybe)
import           Text.Printf (printf)

import AST
import Util

writeFile :: Specifications -> FilePath -> Maybe Values -> IO ()
writeFile specs filePath values =
  let toplevelSpecs = filter toplevel $ Map.elems specs
  in
    IO.withBinaryFile filePath IO.WriteMode $ \handle -> do
      evalStateT (writeSpecifications toplevelSpecs) $ defaultEnv specs handle values

writeSpecifications :: [Specification] -> Write ()
writeSpecifications = \case
  [spec] -> withPrefix 0 $ writeSpecification spec
  specs  -> do
    let choice     = [0..length specs - 1]
        assignment = Just $ zip choice $ map name specs
    i <- choose (fromString "specification") False choice Nothing TypeUInt8 FormatDec assignment
    withPrefix i $ writeSpecification $ specs !! i

writeSpecification :: Specification -> Write ()
writeSpecification specification = do
  mapEnv $ \env -> env { specFilePath = filePath specification }
  writeStatements $ statements specification

writeStatements :: [Statement] -> Write ()
writeStatements = mapM_ writeStatement

writeStatement :: Statement -> Write ()
writeStatement = \case
  StmtExpectConstant t v   -> writeConstant t v
  StmtExpectValue v        -> writeValue v
  StmtExpectEnum e n t f a -> writeEnum e n t f a
  StmtExpectData n l       -> writeData n l
  StmtExpectAscii n l      -> writeAscii n l
  StmtSequence n l s       -> writeSequence n l s
  StmtIf c t f             -> writeIf c t f
  StmtByteOrder b          -> writeByteOrder b
  StmtLet n e              -> writeLet n e
  StmtTry s                -> writeTry s

writeConstant :: Type -> Value -> Write ()
writeConstant t v = writeValueOfType v t

writeValue :: ExpectValue -> Write ()
writeValue = \case
  EVSingle name type_ format assignment -> writeSingleValue False name type_ format assignment

  EVSequence name type_ length format assignment -> do
    length <- writeLength length
    forM_ [0..length - 1] $ \i -> 
      withPrefix i $ writeSingleValue True name type_ format assignment

  EVPacked assignment type_ format -> do
    value <- foldM go 0 assignment
    writeValueOfType value type_
    where
      go value (numBits, name) = do
        value' <- enter name False (bounds $ Left numBits) type_ format
        addValue name value'
        return $ (value `shiftL` numBits) .|. value'

writeSingleValue :: Bool -> BS.ByteString -> Type -> Format -> Maybe Assignment -> Write ()
writeSingleValue showPrefix name type_ format assignment = do
  prefix <- fromEnv prefix
  value <- case assignment of
             Just a -> choose name showPrefix (map fst a) (Just $ bounds $ Right type_) type_ 
                              format assignment
             _      -> enter name showPrefix (bounds $ Right type_) type_ format
  writeValueOfType value type_
  addValue name value

writeEnum :: [Int] -> BS.ByteString -> Type -> Format -> Maybe Assignment -> Write ()
writeEnum enum name type_ format assignment = do
  value <- choose name False enum Nothing type_ format assignment
  writeValueOfType value type_
  addValue name value
    
writeData :: BS.ByteString -> Length -> Write ()
writeData name length = do
  length <- writeLength length
  forM_ [0..length - 1] $ \i ->
    withPrefix i $ writeSingleValue True name TypeUInt8 FormatHex Nothing

writeAscii :: BS.ByteString -> Length -> Write ()
writeAscii name length = do
  printLn BS.empty
  length   <- writeLength length
  default_ <- getDefault  length
  go default_ length
  where
    go default_ length = do
      print $ BS.concat [ fromString $ "Enter ", name
                        , fromString $ " (ascii) of length " ++ show length
                        , formatDefault default_, fromString ": " ]

      line <- runIO IO.getLine >>= \case
        "" | isJust default_ -> return $ BS.take length $ fromJust default_
        line                 -> return $ fromString $ read ("\"" ++ take length line ++ "\"")

      writeChunk line
      when (length > BS.length line) $ go Nothing $ length - (BS.length line)

    formatDefault = \case
      Nothing -> BS.empty
      Just d  -> BS.concat [fromString " [", d, fromString "]"]

    getDefault length = do
      values <- forM [0..length - 1] $ \i -> withPrefix i $ optionalValueFromEnv name
      return $ fmap BS.pack $ sequence $ map (fmap fromIntegral) values

writeSequence :: BS.ByteString -> Maybe Length -> [Statement] -> Write ()
writeSequence name length statements = mapM writeLength length >>= go 0 
  where
    go i = \case
      Nothing -> do
        oneMore <- withPrefix i $ choose label True [0,1] Nothing TypeUInt8 FormatDec assignment
        when (oneMore == 1) $ do
          withPrefix i $ writeStatements statements
          go (i + 1) Nothing

      Just length | i < length -> do
        withPrefix i $ writeStatements statements
        go (i + 1) $ Just length

      Just _ -> return ()

    label      = BS.append (fromString "New ") name
    assignment = Just [ (0, fromString "No"), (1, fromString "Yes") ]

writeIf :: Expression -> [Statement] -> Maybe [Statement] -> Write ()
writeIf condition true mFalse = do
  c <- writeExpression condition
  if c > 0 then withPrefix 0 $ writeStatements true
           else case mFalse of
                  Nothing    -> return ()
                  Just false -> withPrefix 1 $ writeStatements false

writeByteOrder :: ByteOrder -> Write ()
writeByteOrder = \case
  ByteOrderBigEndian    -> mapEnv $ \env -> env { byteOrder = E.BigEndian }
  ByteOrderLittleEndian -> mapEnv $ \env -> env { byteOrder = E.LittleEndian }
  ByteOrderSwap         -> mapEnv $ \env -> 
                             case byteOrder env of
                               E.BigEndian    -> env { byteOrder = E.LittleEndian }
                               E.LittleEndian -> env { byteOrder = E.BigEndian }

writeLet :: BS.ByteString -> Expression -> Write ()
writeLet name expr = do
  value <- writeExpression expr
  addValue name value

writeTry :: [BS.ByteString] -> Write ()
writeTry names = do
  specs <- forM names $ \n ->
             fromEnv specs >>= return . Map.lookup n >>= \case
               Just s  -> return s
               Nothing -> failSpec $ "Could not find specification " ++ (show n)
  writeSpecifications specs

writeLength :: Length -> Write Int
writeLength = \case
 LengthConstant c -> return c
 LengthVariable v -> valueFromEnv v

writeExpression :: Expression -> Write Int
writeExpression e = do
  prefix <- fromEnv prefix
  value  <- fromEnv values >>= return . evaluate e prefix
  case value of
    Nothing -> failSpec "Could not evaluate expression"
    Just v  -> return v

enter :: BS.ByteString -> Bool -> (Value, Value) -> Type -> Format -> Write Value
enter name showPrefix bounds type_ format = go
  where
    go = do
      promptName <- if showPrefix
                    then fromEnv prefix >>= \(p:_) -> return $ toPrefixName name [p]
                    else return name

      default_ <- optionalValueFromEnv name
      print $ BS.concat [ fromString $ "Enter ", promptName, fromString " "
                        , formatBounds  bounds   type_ format
                        , formatDefault default_ type_ format Nothing
                        , fromString ": "]
      runIO IO.getLine >>= \case
        "" | isJust default_ -> return $ fromJust default_
        line                 -> case readMaybe line of
          Just i | inBounds i bounds -> return i
          _                          -> go

choose :: BS.ByteString -> Bool -> [Value] -> Maybe (Value, Value) -> Type 
       -> Format -> Maybe Assignment -> Write Value
choose name showPrefix choice allowCustom type_ format assignment = do
  printLn BS.empty
  forM_ choice $ \c ->
    let key   = formatValue c type_ format Nothing
        value = case assignment of
                  Nothing -> fromString ""
                  Just a  -> case lookup c a of
                    Nothing -> fromString ""
                    Just n  -> n
    in
      printLn $ BS.concat [name, fromString " ", key, fromString ": ", value]
  go
  where
    go = do
      promptName <- if showPrefix
                    then fromEnv prefix >>= \(p:_) -> return $ toPrefixName name [p]
                    else return name

      print $ BS.append (fromString "Choose ") promptName
      default_ <- optionalValueFromEnv name
      case allowCustom of
        Nothing     -> print $ BS.concat [ formatDefault default_ type_ format assignment
                                         , fromString ": " ]
        Just bounds -> print $ BS.concat [ fromString " or enter custom value "
                                         , formatBounds bounds type_ format
                                         , formatDefault default_ type_ format assignment
                                         , fromString ": " ]
      runIO IO.getLine >>= \case
        "" | isJust default_ -> return $ fromJust default_
        line                 -> case readMaybe line of
          Nothing -> go
          Just i  -> case allowCustom of
            Just bounds -> if inBounds i bounds then return i else go
            Nothing     -> if i `elem` choice   then return i else go

formatBounds :: (Value, Value) -> Type -> Format -> BS.ByteString
formatBounds (min, max) type_ format =
  BS.concat [ fromString "(", formatValue min type_ format Nothing
            , fromString "-", formatValue max type_ format Nothing
            , fromString ")" ]

formatDefault :: Maybe Value -> Type -> Format -> Maybe Assignment -> BS.ByteString
formatDefault default_ type_ format assignment = case default_ of
  Nothing -> BS.empty
  Just d  -> BS.concat [fromString " [", formatValue d type_ format assignment, fromString "]"]

inBounds :: Value -> (Value, Value) -> Bool
inBounds value (min, max) = (min <= value) && (value <= max)

writeValueOfType :: Value -> Type -> Write ()
writeValueOfType value type_ = case type_ of
  TypeUInt8    -> writeNum value B.putWord8    B.putWord8
  TypeUInt16   -> writeNum value B.putWord16le B.putWord16be
  TypeUInt32   -> writeNum value B.putWord32le B.putWord32be
  TypeInt8     -> writeNum value B.putInt8     B.putInt8
  TypeInt16    -> writeNum value B.putInt16le  B.putInt16be
  TypeInt32    -> writeNum value B.putInt32le  B.putInt32be

writeNum :: Integral a => Int -> (a -> B.Put) -> (a -> B.Put) -> Write ()
writeNum num littleEndian bigEndian = do
  byteOrder <- fromEnv byteOrder
  writeChunk $ LazyBS.toStrict $ B.runPut $ case byteOrder of
    E.LittleEndian -> littleEndian $ fromIntegral num
    E.BigEndian    -> bigEndian    $ fromIntegral num

writeChunk :: BS.ByteString -> Write ()
writeChunk chunk = do
  handle <- fromEnv handle
  runIO $ BS.hPut handle chunk

withPrefix :: Int -> Write a -> Write a
withPrefix p run = do
  mapEnv $ \env -> env { prefix = p : (prefix env) }
  result <- run
  mapEnv $ \env -> env { prefix = tail $ prefix env }
  return result

addValue :: BS.ByteString -> Value -> Write ()
addValue name value = do
  prefix <- fromEnv prefix
  mapEnv $ \env -> env { values = Map.insert (name, prefix) value $ values env }

mapEnv :: (Env -> Env) -> Write ()
mapEnv = modify'

valueFromEnv :: BS.ByteString -> Write Value
valueFromEnv name =
  optionalValueFromEnv name >>= \case
    Just value -> return value
    Nothing    -> failSpec $ "Could not find variable " ++ (show name)

optionalValueFromEnv :: BS.ByteString -> Write (Maybe Value)
optionalValueFromEnv name = do
  prefix <- fromEnv prefix
  fromEnv values >>= return . getValue name prefix

printLn :: BS.ByteString -> Write ()
printLn b = do
  print b
  print $ fromString "\n"

print :: BS.ByteString -> Write ()
print = runIO . BS.putStr

failSpec :: String -> Write a
failSpec msg = do
  specFilePath <- fromEnv specFilePath
  runIO $ error $ concat [specFilePath, ": ", msg]

runIO :: IO a -> Write a
runIO = lift

fromEnv :: (Env -> a) -> Write a
fromEnv = gets

type Write a = StateT Env IO a

defaultEnv :: Specifications -> IO.Handle -> Maybe Values -> Env
defaultEnv specs handle values = 
  Env specs handle (fromMaybe Map.empty values) "" E.getSystemEndianness []

data Env = Env { specs        :: Specifications
               , handle       :: IO.Handle
               , values       :: Values
               , specFilePath :: FilePath
               , byteOrder    :: E.Endianness
               , prefix       :: [Int]
               }

