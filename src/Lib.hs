module Lib
    ( format
    ) where

import Prelude

import Language.PureScript.CST

format :: Module () -> Module ()
format = id
