{-# LANGUAGE RecordWildCards #-}
module Lib
    ( format
    ) where

import Relude

import Language.PureScript.CST.Errors
import Language.PureScript.CST.Print
import Language.PureScript.CST as CST
import Data.Generics.Product
import Control.Lens
import qualified Data.Text as T

import Psfmt.Imports
import Psfmt.RecordAliases
import Psfmt.Utils
import Psfmt.Traverals.TraverseSource
import Psfmt.Traverals.TraverseStyle

format :: Text -> Either (NonEmpty ParserError) Text
format input = do
  parsed <- parse input
  let
    extractSource = toListOf traverseSourceToken
    tokens = printTokens $ unicodePass $ trailingWhitespacePass $ extractSource $ formatModule parsed
  pure $ tokens <> foldMap ppLc (modTrailingComments parsed)

ppLc :: Comment LineFeed -> Text
ppLc = \case
  Comment raw -> raw
  Space n -> T.replicate n " "
  Line LF -> "\n"
  Line CRLF -> "\r\n"

formatModule :: Module () -> Module ()
formatModule Module {..} =
  Module
     { modAnn = modAnn
     , modKeyword = modKeyword
     , modNamespace = modNamespace
     , modExports = modExports
     , modWhere = modWhere
     , modImports = sortImports modImports
     , modDecls = void . reformatDecl . fmap (\_ -> IndentLevel 0) <$> modDecls
     , modTrailingComments = modTrailingComments
     }

reformatDecl :: Declaration IndentLevel -> Declaration IndentLevel
reformatDecl (DeclType a b c d) = reformatTypeDecl a b c d
reformatDecl decl = decl

trailingWhitespacePass :: [SourceToken] -> [SourceToken]
trailingWhitespacePass tokens =
  fmap fun zippedList
  where
    zippedList = zip tokens (map Just (drop 1 tokens) ++ [Nothing])
    fun :: (SourceToken, Maybe SourceToken) -> SourceToken
    fun (current, next) =
      if maybe True (any isNewline . tokLeadingComments . tokAnn) next
      then
        trailWith removeWhiteSpace current
      else
        current

unicodePass :: [SourceToken] -> [SourceToken]
unicodePass = fmap (set traversSourceStyle Unicode)
