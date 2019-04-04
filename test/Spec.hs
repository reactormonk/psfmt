module Main where

import Relude

import Test.Tasty
import Test.Tasty.Golden

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Language.PureScript.CST
import Language.PureScript.CST.Errors
import Language.PureScript.CST.Print

import Data.Generics.Product
import Control.Lens
import System.FilePath
import Control.Exception

import TraverseTree
import Lib

main :: IO ()
main = do
  t <- tests
  defaultMain t

tests :: IO TestTree
tests = do
  goldens <- goldenTests
  pure $ testGroup "Golden Tests" goldens

goldenTests :: IO [TestTree]
goldenTests = do
  allFiles <- liftIO $ findByExtension [".purs"] "test/data"
  let files = filter (\e -> ".out" == (takeExtension $ dropExtension e)) allFiles
  traverse createGoldenTest files

instance Exception [ParserError]

createGoldenTest :: FilePath -> IO TestTree
createGoldenTest goldenPath = do
  let inputPath = addExtension (dropExtension $ dropExtension goldenPath) ".purs"
  input <- T.readFile inputPath
  pure $ goldenVsString inputPath goldenPath $
    case processText format input of
      Left e -> throw e
      Right out -> pure $ toLazy $ (encodeUtf8 :: Text -> ByteString) out

processText :: (Module () -> Module ()) -> Text -> Either [ParserError] Text
processText fun input = do
  parsed <- parse input
  let tokens = printTokens $ extractSource $ fun parsed
  pure $ tokens <> foldMap ppLc (modTrailingComments parsed)

ppLc :: Comment LineFeed -> Text
ppLc = \case
  Comment raw -> raw
  Space n -> T.replicate n " "
  Line LF -> "\n"
  Line CRLF -> "\r\n"
