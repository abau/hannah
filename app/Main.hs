{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (forM)
import Data.Maybe (catMaybes)
import System.Exit (exitFailure)
import System.Directory (listDirectory)
import System.FilePath ((</>))

import AST (Specification)
import Paths_hannah (getDataDir)
import ParseSpec
import ParseOption
import Read (trySpecificationsOnFile)

main :: IO ()
main = do
  option <- parseOption
  specs  <- parseSpecifications
  case mode option of
    Write -> undefined
    Read  -> trySpecificationsOnFile specs (filePath option) >>= \case
      True  -> return ()
      False -> putStrLn ("Could not parse '" ++ filePath option ++ "'") >> exitFailure

parseSpecifications :: IO [Specification]
parseSpecifications = do
  dataDir <- getDataDir
  files   <- listDirectory (dataDir </> "specs")
  specs   <- forM files $ \f ->
              parseSpecFile $ dataDir </> "specs" </> f
  return $ catMaybes specs
