{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad (forM)
import qualified Data.Map.Strict as Map
import           Data.String (fromString)
import           Prelude hiding (writeFile)
import           System.Exit (exitFailure)
import           System.Directory (listDirectory, doesDirectoryExist)
import           System.FilePath ((</>), takeBaseName)

import AST (Specification (Specification), Specifications)
import JavaScript (writeJavaScript)
import Paths_hannah (getDataDir)
import ParseSpec (parseSpecFile)
import ParseOption
import Read (trySpecificationsOnFile)
import Write (writeFile)

main :: IO ()
main = do
  option <- parseOption
  specs  <- parseSpecifications ""
  case option of
    OptionWrite file -> writeFile specs file Nothing

    OptionRead file debug -> trySpecificationsOnFile specs file True debug >>= \case
      Just _  -> return ()
      Nothing -> putStrLn ("Could not parse '" ++ file ++ "'") >> exitFailure

    OptionModify inFile outFile -> trySpecificationsOnFile specs inFile False False >>= \case
      Nothing -> putStrLn ("Could not parse '" ++ inFile ++ "'") >> exitFailure
      Just v  -> writeFile specs outFile $ Just v

    OptionJavaScript -> do
      html <- getDataDir >>= \d -> return (d </> "hannah-js.html")
      writeJavaScript specs html

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
