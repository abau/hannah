{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad (forM)
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import           System.Exit (exitFailure)
import           System.Directory (listDirectory, doesDirectoryExist)
import           System.FilePath ((</>), takeBaseName)

import AST (Specification (Specification), Specifications)
import Paths_hannah (getDataDir)
import ParseSpec (parseSpecFile)
import ParseOption
import Read (trySpecificationsOnFile)

main :: IO ()
main = do
  option <- parseOption
  specs  <- parseSpecifications ""
  case mode option of
    Write -> undefined
    Read  -> trySpecificationsOnFile specs (filePath option) >>= \case
      True  -> return ()
      False -> putStrLn ("Could not parse '" ++ filePath option ++ "'") >> exitFailure

parseSpecifications :: FilePath -> IO Specifications
parseSpecifications relativeFilePath = do
  filePath <- getDataDir >>= \d -> return (d </> "specs" </> relativeFilePath)
  content  <- listDirectory filePath
  specs    <- forM content $ \c ->
                let filePath' = filePath </> c
                    name      = relativeFilePath </> (takeBaseName c) 
                    toplevel  = relativeFilePath == ""
                in do
                  isDir <- doesDirectoryExist filePath'
                  if isDir 
                    then parseSpecifications name
                    else parseSpecFile filePath' >>= \case
                      Nothing    -> return Map.empty
                      Just stmts -> 
                        let spec = Specification filePath' (fromString name) toplevel stmts
                        in
                          return $ Map.singleton (fromString name) spec
  return $ Map.unions specs
