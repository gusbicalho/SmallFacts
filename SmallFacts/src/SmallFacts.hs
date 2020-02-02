{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module SmallFacts
  ( runSmallFacts
  ) where

runSmallFacts :: m a -> m a
runSmallFacts = id
