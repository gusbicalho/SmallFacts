module SmallFacts.Core.KeyValStore.Effect
  ( KeyValStoreRead (..)
  , KeyValStoreWrite (..)
  , KeyValStore
  , KVPutResult (..)
  , kvGet
  , kvGetOrPut
  , kvPut
  -- * Re-exports
  , Algebra
  , Effect
  , Has
  , run
  ) where

import Control.Algebra
import GHC.Generics (Generic1)

data KeyValStoreRead key val m k
  = KVGet key (Maybe val -> m k)
  deriving (Functor, Generic1, HFunctor, Effect)

data KeyValStoreWrite key val m k
  = KVPut key val (KVPutResult val -> m k)
  deriving (Functor, Generic1, HFunctor, Effect)

type KeyValStore key val e = KeyValStoreRead key val :+: KeyValStoreWrite key val

data KVPutResult val
  = KVPutOk val
  | KVPutConflict val

fromPutResult :: KVPutResult p -> p
fromPutResult (KVPutOk val) = val
fromPutResult (KVPutConflict val) = val

kvGet :: Has (KeyValStoreRead key val) sig m => key -> m (Maybe val)
kvGet key = send $ KVGet key pure

kvPut :: Has (KeyValStoreWrite key val) sig m => key -> val -> m (KVPutResult val)
kvPut key val = send $ KVPut key val pure

kvGetOrPut :: Has (KeyValStoreWrite key val) sig m => key -> val -> m val
kvGetOrPut key val = fromPutResult <$> kvPut key val
