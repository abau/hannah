{-# LANGUAGE LambdaCase #-}
module ParseOption (Option (..), parseOption) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

data Option = OptionRead   FilePath
            | OptionWrite  FilePath
            | OptionModify FilePath FilePath

parseOption :: IO Option
parseOption =
  getArgs >>= \case 
    ["read"  , inF]       -> return $ OptionRead   inF
    ["write" , outF]      -> return $ OptionWrite  outF
    ["modify", inF, outF] -> return $ OptionModify inF outF
    _                     -> printUsage >> exitFailure

printUsage :: IO ()
printUsage = do
  name <- getProgName
  putStrLn $ concat ["Usage: ", name, " (read FILE | write FILE | modify IN_FILE OUT_FILE)"]
