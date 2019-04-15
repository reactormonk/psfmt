{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -freduction-depth=0 #-}

module Psfmt.Traverals.TraverseStyle where

import Relude

import Language.PureScript.CST

import Data.Generics.Product
import Control.Lens

data Custom
type instance Children Custom a = ChildrenCustom a

type family ChildrenCustom (a :: Relude.Type) where
  ChildrenCustom Text = '[]
  ChildrenCustom a = Children ChGeneric a

-- takes about 2 minutes to compile, so it's in a separate file.
traverseToken :: Traversal' (Module ()) Token
traverseToken = typesUsing @Custom @Token @(Module ())

traverseSourceStyleT :: Traversal' Token SourceStyle
traverseSourceStyleT = typesUsing @Custom @SourceStyle @Token

traversSourceStyle :: Traversal' (Module ()) SourceStyle
traversSourceStyle = traverseToken . traverseSourceStyleT
