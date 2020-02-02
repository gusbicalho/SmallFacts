module SmallFacts.Core.Types
 ( IndexKey (..)
 ) where

import Data.UUID

data IndexKey
  = RootRef
  | Node UUID
