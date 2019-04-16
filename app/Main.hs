module Main where

import Lib
import System.Environment
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Language.PureScript.CST.Errors
import System.Directory
import System.Directory.PathWalk
import Data.List (isSuffixOf)
import System.FilePath

import Relude

main :: IO ()
main = do
  args <- getArgs
  let
    dirsOrFiles = if length args > 0 then args else ["."]
    findMoreFiles directory = do
      pathWalkAccumulate directory $ \dir subdirs files ->
        pure $ fmap (\f -> dir </> f) $ filter (isSuffixOf ".purs") files
  files <- filterM doesFileExist dirsOrFiles
  dirs <- filterM doesDirectoryExist dirsOrFiles
  filesInDirs <- fmap join $ traverse findMoreFiles dirs
  let toFormat = files <> filesInDirs
  traverse_ formatFile toFormat
  T.hPutStr stdout $ "Formatted " <> (show $ length toFormat) <> " files\n"

formatFile :: FilePath -> IO ()
formatFile filename = do
  content <- T.readFile filename
  case format content of
    Left e ->
      T.hPutStr stderr $
        "Errors while formatting file " <> (T.pack filename) <> ": " <> (T.pack $ show e)
    Right output ->
      T.writeFile filename output
