module SmallFacts.Core.BTree.Partitions
  ( Partitions
  , PartitionIndex
  , findPart
  , spliceReplacingAt
  , twoParts
  ) where

import qualified Data.Vector as V
import           Data.Vector (Vector)

data PartitionIndex
  = First
  | MiddleIndex Int
  | Last
  deriving (Eq, Show, Ord)

data Partitions key val
  = Partitions
  { partitionsFirst :: (val, key)
  , partitionsMiddle :: Vector (val, key)
  , partitionsLast :: val
  }

findPart :: Ord key => key -> Partitions key val -> (PartitionIndex, val)
findPart key (Partitions (firstVal, firstKey) middle lastVal)
  | firstKey > key            = (First, firstVal)
  | V.length middle == 0      = (Last, lastVal)
  | snd (V.last middle) < key = (Last, lastVal)
  | otherwise = go (Last, lastVal) (V.length middle - 1)
    where
      go _ (-1) = (First, firstVal)
      go acc i  =
        let (iVal, iKey) = middle V.! i
        in if iKey < key
           then acc
           else go (MiddleIndex i, iVal) (i - 1)

spliceReplacingAt :: PartitionIndex
                  -> Partitions key val
                  -> Partitions key val
                  -> Partitions key val
spliceReplacingAt First
                  (Partitions newFirst      newMiddle newLastVal)
                  (Partitions (_, firstKey) middle    lastVal)
  = Partitions newFirst
               (newMiddle <>
                V.singleton (newLastVal, firstKey) <>
                middle)
               lastVal
spliceReplacingAt Last
                  (Partitions newFirst newMiddle newLastVal)
                  (Partitions first    middle    _)
  = Partitions first
               (middle <>
                V.singleton newFirst <>
                newMiddle)
               newLastVal
spliceReplacingAt (MiddleIndex i)
                  pa@(Partitions newFirst newMiddle newLastVal)
                  pb@(Partitions first    middle    lastVal)
  = let (beforeI, fromI) = V.splitAt i middle
    in case splitVectorHead fromI of
      Nothing -> spliceReplacingAt Last pa pb
      Just ((_, iKey), afterI) ->
        Partitions first
                   (beforeI <>
                    V.singleton newFirst <>
                    newMiddle <>
                    V.singleton (newLastVal, iKey) <>
                    afterI)
                   lastVal

splitVectorHead :: Vector a -> Maybe (a, Vector a)
splitVectorHead (V.splitAt 0 -> (front, back)) =
  if V.length front == 0
  then Nothing
  else Just (V.head front, back)

twoParts :: val -> key -> val -> Partitions key val
twoParts v1 key v2 = Partitions (v1, key) V.empty v2
