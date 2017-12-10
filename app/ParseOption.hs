{-# LANGUAGE LambdaCase #-}
module ParseOption (Option (..), parseOption) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

data Option = OptionRead       FilePath Bool
            | OptionWrite      FilePath
            | OptionModify     FilePath FilePath
            | OptionJavaScript

parseOption :: IO Option
parseOption =
  getArgs >>= \case 
    ["read"  , inF]          -> return $ OptionRead   inF False
    ["read"  , "debug", inF] -> return $ OptionRead   inF True
    ["write" , outF]         -> return $ OptionWrite  outF
    ["modify", inF, outF]    -> return $ OptionModify inF outF
    ["javascript"]           -> return $ OptionJavaScript
    _                        -> printUsage >> exitFailure

printUsage :: IO ()
printUsage = do
  name <- getProgName
  putStrLn $ concat ["Usage: ", name, " (read [debug] FILE | write FILE | modify IN_FILE OUT_FILE | javascript)"]
