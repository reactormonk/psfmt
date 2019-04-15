module Psfmt.Utils where

import Relude

import Language.PureScript.CST as CST

import Data.Generics.Product
import Control.Lens

isWhitespace :: Comment a -> Bool
isWhitespace (Comment _) = False
isWhitespace (Space _) = True
isWhitespace (Line _) = True

isNewline :: Comment a -> Bool
isNewline (Comment _) = False
isNewline (Space _) = False
isNewline (Line _) = True

replaceWhitespaceWith :: [Comment l] -> [Comment l] -> [Comment l]
replaceWhitespaceWith replacement target =
  if all isWhitespace target then
    replacement
  else
    target

replaceWith1Space :: [Comment l] -> [Comment l]
replaceWith1Space = replaceWhitespaceWith [Space 1]

replaceWithLF :: [Comment LineFeed] -> [Comment LineFeed]
replaceWithLF = replaceWhitespaceWith [Line LF]

replaceWith2LF :: [Comment LineFeed] -> [Comment LineFeed]
replaceWith2LF = replaceWhitespaceWith [Line LF, Line LF]

removeWhiteSpace :: [Comment l] -> [Comment l]
removeWhiteSpace = replaceWhitespaceWith []

trimWhiteSpace :: SourceToken -> SourceToken
trimWhiteSpace s =
  over (field @"tokAnn" . field @"tokLeadingComments") removeWhiteSpace $
  over (field @"tokAnn" . field @"tokTrailingComments") replaceWith1Space $
  s

newtype IndentLevel = IndentLevel Int -- *2 for distance
  deriving (Show, Num)

indentWith :: IndentLevel -> SourceToken -> SourceToken
indentWith (IndentLevel i) = leadWith (replaceWhitespaceWith [Line LF, Space (i*2)])

leadWith :: ([Comment LineFeed] -> [Comment LineFeed]) -> SourceToken -> SourceToken
leadWith fun = over (field @"tokAnn" . field @"tokLeadingComments") fun

trailWith :: ([Comment Void] -> [Comment Void]) -> SourceToken -> SourceToken
trailWith fun = over (field @"tokAnn" . field @"tokTrailingComments") fun

indentWrapped :: IndentLevel -> CST.Wrapped a -> CST.Wrapped a
indentWrapped i =
  over (field @"wrpOpen") (indentWith i) .
  over (field @"wrpClose") (indentWith i) .
  trimWrapped

trimWrapped :: CST.Wrapped a -> CST.Wrapped a
trimWrapped w =
  over (field @"wrpOpen") trimWhiteSpace $
  over (field @"wrpClose") trimWhiteSpace $
  w
