{-# LANGUAGE RecordWildCards #-}
module Lib
    ( format
    ) where

import Relude

import Data.List
import Language.PureScript.CST
import Data.Generics.Product
import Control.Lens

import Psfmt.Imports

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
