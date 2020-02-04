module SmallFacts.Core.BTree.Play where

import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           GHC.TypeLits
import qualified Data.Vector as V
import           Data.Vector (Vector)
import qualified SmallFacts.Core.BTree.Partitions as Parts

newtype Leaf (n :: Nat) key val
  = Leaf { unLeaf :: Vector (key, val) }

newtype Internal (n :: Nat) key val
  = Internal { unInternal :: Parts.Partitions key val }
