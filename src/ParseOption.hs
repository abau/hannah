{-# LANGUAGE LambdaCase #-}
module ParseOption (Option (..), Mode (..), parseOption) where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

data Mode = Read | Write

data Option = Option { mode     :: Mode
                     , filePath :: FilePath
                     }

parseOption :: IO Option
parseOption =
  getArgs >>= \case 
    ["read", filePath]  -> return $ Option Read filePath
    ["write", filePath] -> return $ Option Write filePath
    _                   -> printUsage >> exitFailure

printUsage :: IO ()
printUsage = do
  name <- getProgName
  putStrLn $ concat ["Usage: ", name, ": (read|write) FILE"]
