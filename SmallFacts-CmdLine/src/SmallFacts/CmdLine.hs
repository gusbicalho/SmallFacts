{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module SmallFacts.CmdLine
  ( start
  ) where

import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Carrier.Trace.Printing
import Data.Function
import SmallFacts

start :: IO ()
start = app
      & runSmallFacts
      & runTrace
      & runReader (42 :: Int)
      & runM @IO

app :: _ => m ()
app = do
  trace "Running app"
  answer <- ask @Int
  trace $ "The answer is " <> show answer
