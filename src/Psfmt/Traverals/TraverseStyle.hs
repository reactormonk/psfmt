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

traversSourceStyle :: Traversal' (SourceToken) SourceStyle
traversSourceStyle = typesUsing @Custom @SourceStyle @SourceToken
