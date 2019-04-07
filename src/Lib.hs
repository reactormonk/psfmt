{-# LANGUAGE RecordWildCards #-}
module Lib
    ( format
    ) where

import Relude

import Data.List
import Language.PureScript.CST
import Data.Generics.Product
import Control.Lens

format :: Module () -> Module ()
format Module {..} =
  Module
     { modAnn = modAnn
     , modKeyword = modKeyword
     , modNamespace = modNamespace
     , modExports = modExports
     , modWhere = modWhere
     , modImports = sortImports modImports
     , modDecls = modDecls
     , modTrailingComments = modTrailingComments
     }

sortImports :: [ImportDecl a] -> [ImportDecl a]
sortImports decls =
  let
    (specific, unspecific) = partition fun decls
    fun imp = isJust (impNames imp) || isJust (impQual imp)
    sorter = sortWith (\imp -> (identQual $ impModule imp, identName $ impModule imp))
    cleanWhitespace decl =
      if leadingWhitespaceOnly $ impKeyword decl
      then set ((field @"impKeyword") . (field @"tokAnn"). (field @"tokLeadingComments")) [Line LF] decl
      else decl
    addLineFeed decl =
      over ((field @"impKeyword") . (field @"tokAnn") . (field @"tokLeadingComments")) (\l -> l ++ [Line LF]) decl
    oneLinePre decls' =
      over (ix 0) addLineFeed $ map cleanWhitespace decls'
  in
    (oneLinePre $ sorter unspecific) ++ (oneLinePre $ sorter specific)

leadingWhitespaceOnly :: SourceToken -> Bool
leadingWhitespaceOnly st = all isWhitespace $ tokLeadingComments $ tokAnn st

isWhitespace :: Comment a -> Bool
isWhitespace (Comment _) = False
isWhitespace (Space _) = True
isWhitespace (Line _) = True
