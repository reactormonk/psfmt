module Main where

import Lib
import System.Environment
import Data.Text.IO as T
import Relude

main :: IO ()
main = do
  args <- getArgs
  let filename = "filename"
  content <- T.readFile filename
  T.writeFile filename content
