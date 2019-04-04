{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module TraverseTree where

import Relude

import Language.PureScript.CST

import Data.Generics.Product
import Control.Lens

-- tajes about 2 minutes to compile, so it's in a separate file.
extractSource :: Module () -> [SourceToken]
extractSource m = toListOf (typesUsing @Custom @SourceToken @(Module ())) m

data Custom
type instance Children Custom a = ChildrenCustom a

type family ChildrenCustom (a :: Relude.Type) where
  ChildrenCustom Text = '[]
  ChildrenCustom a = Children ChGeneric a
