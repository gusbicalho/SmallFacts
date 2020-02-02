{-# LANGUAGE UndecidableInstances #-}
module SmallFacts.Core.KeyValStore.Carrier.InMemoryList where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent.MVar
import SmallFacts.Core.KeyValStore.Effect

newtype LookupList key val = LookupList [(key, val)]
type LookupVar key val = MVar (LookupList key val)

newtype InMemoryListKeyValStoreC key val m a = InMemoryListKeyValStoreC {
  runInMemoryListKeyValStoreC :: ReaderC (MVar (LookupList key val)) m a
  } deriving newtype (Functor, Applicative, Monad)

instance ( Effect sig
         , Algebra sig m
         , Has (Lift IO) sig m
         , Eq key
         , Eq val
         ) => Algebra (KeyValStoreRead key val :+: KeyValStoreWrite key val :+: sig) (InMemoryListKeyValStoreC key val m) where
  alg (L (KVGet key k)) = (k =<<) . InMemoryListKeyValStoreC $ do
    LookupList table <- sendM . readMVar =<< ask @(LookupVar key val)
    pure $ lookup key table
  alg (R (L (KVPut key val k))) = (k =<<) . InMemoryListKeyValStoreC $ do
    lookupVar <- ask @(LookupVar key val)
    sendM $ modifyMVar lookupVar $ \(LookupList table) ->
      pure $
        case lookup key table of
          Nothing ->         (LookupList $ (key, val) : table, KVPutOk val)
          Just val'
            | val' == val -> (LookupList                table, KVPutOk val')
            | otherwise   -> (LookupList                table, KVPutConflict val')
  alg (R (R other)) = InMemoryListKeyValStoreC (alg (R (handleCoercible other)))
