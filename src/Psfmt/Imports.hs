module Psfmt.Imports where

import Relude

import Data.List
import Language.PureScript.CST
import Data.Generics.Product
import Control.Lens
import Psfmt.Utils

sortImports :: [ImportDecl a] -> [ImportDecl a]
sortImports decls =
  let
    (specific, unspecific) = partition fun decls
    fun imp = isJust (impNames imp) || isJust (impQual imp)
    sorter = sortWith (\imp -> (nameValue $ impModule imp))
    cleanWhitespace decl =
      if leadingWhitespaceOnly $ impKeyword decl
      then set ((field @"impKeyword") . (field @"tokAnn") . (field @"tokLeadingComments")) [Line LF] decl
      else decl
    addLineFeed decl =
      over ((field @"impKeyword") . (field @"tokAnn") . (field @"tokLeadingComments")) (\l -> l ++ [Line LF]) decl
    oneLinePre decls' =
      over (ix 0) addLineFeed $ map cleanWhitespace decls'
  in
    (oneLinePre $ sorter unspecific) ++ (oneLinePre $ sorter specific)

leadingWhitespaceOnly :: SourceToken -> Bool
leadingWhitespaceOnly st = all isWhitespace $ tokLeadingComments $ tokAnn st
