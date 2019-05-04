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
import Text.Pretty.Simple

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
  let files = filter (\e -> ".out" /= (takeExtension $ dropExtension e)) allFiles
  traverse createGoldenTest files

instance Exception (NonEmpty ParserError)

createGoldenTest :: FilePath -> IO TestTree
createGoldenTest inputPath = do
  let goldenPath = addExtension (dropExtension inputPath) ".out.purs"
  input <- T.readFile inputPath
  pure $ goldenVsStringDiff goldenPath (\ref new -> ["diff", "-u", ref, new]) goldenPath $
    case format input of
      Left e -> throw e
      Right out -> pure $ toLazy $ (encodeUtf8 :: Text -> ByteString) out

tracePShowId :: Show a => a -> a
tracePShowId a = trace (pShow a) a
