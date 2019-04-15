{-# LANGUAGE RecordWildCards #-}
module Psfmt.RecordAliases where

import Relude

import Data.List
import Language.PureScript.CST as CST
import Language.PureScript.CST.Types as CST
import Data.Generics.Product
import Control.Lens
import Psfmt.Utils

reformatTypeDecl :: IndentLevel -> DataHead IndentLevel -> SourceToken -> CST.Type IndentLevel -> Declaration IndentLevel
reformatTypeDecl i h st t =
  DeclType
    i
    (reformatHead h)
    (trimWhiteSpace st)
    (reformatType t)

reformatType :: CST.Type IndentLevel -> CST.Type IndentLevel
reformatType (TypeRecord a' w) = reformatRecord (a' + 1) (fmap (fmap (+1)) w)
reformatType t = t

reformatHead :: DataHead a -> DataHead a
reformatHead DataHead {..} =
  DataHead
  { dataHdKeyword = leadWith replaceWith2LF $ trimWhiteSpace dataHdKeyword
  , dataHdName = over (field @"nameTok") trimWhiteSpace dataHdName
  , dataHdVars = dataHdVars -- TODO
  }

reformatRecord :: IndentLevel -> (CST.Wrapped (Row IndentLevel)) -> CST.Type IndentLevel
reformatRecord i w =
  TypeRecord i
    ( indentWrapped i $
      over (field @"wrpValue") (reformatRow i) $ w
    )

reformatRow :: IndentLevel -> Row IndentLevel -> Row IndentLevel
reformatRow i Row {..} = Row
  { rowLabels = fmap (reformatRowLabels i) rowLabels
  , rowTail = fmap reformatRowTail rowTail
  }

reformatRowLabels :: IndentLevel -> Separated (Labeled CST.Label (CST.Type IndentLevel)) -> Separated (Labeled CST.Label (CST.Type IndentLevel))
reformatRowLabels i Separated {..} = Separated
  { sepHead = reformatRowLabel sepHead
  , sepTail = fmap (reformatTail i) sepTail
  }

reformatTail :: IndentLevel -> (SourceToken, (Labeled CST.Label (CST.Type IndentLevel))) -> (SourceToken, (Labeled CST.Label (CST.Type IndentLevel)))
reformatTail i (token, label) =
  ( indentWith i $ trimWhiteSpace token
  , reformatRowLabel label)

reformatRowLabel :: Labeled CST.Label (CST.Type IndentLevel) -> Labeled CST.Label (CST.Type IndentLevel)
reformatRowLabel Labeled {..} = Labeled
  { lblLabel = over (field @"lblTok") trimWhiteSpace lblLabel
  , lblSep = trimWhiteSpace lblSep
  , lblValue = reformatType lblValue
  }

reformatRowTail :: (SourceToken, CST.Type a) -> (SourceToken, CST.Type a)
reformatRowTail = identity -- TODO
