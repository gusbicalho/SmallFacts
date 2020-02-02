{-# LANGUAGE UndecidableInstances #-}
module SmallFacts.Core.KeyValStore.Carrier.InMemoryList
  ( InMemoryListKeyValStoreC
  , MutableLookup
  , runInMemoryListKeyValStore
  , runNewInMemoryListKeyValStore
  , newEmptyState
  ) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.Reader
import Control.Concurrent.MVar
import SmallFacts.Core.KeyValStore.Effect

newtype MutableLookup key val = LookupVar { unLookupVar :: MVar [(key, val)] }

newtype InMemoryListKeyValStoreC key val m a = InMemoryListKeyValStoreC {
  runInMemoryListKeyValStoreC :: ReaderC (MutableLookup key val) m a
  } deriving newtype (Functor, Applicative, Monad)

instance ( Effect sig
         , Algebra sig m
         , Has (Lift IO) sig m
         , Eq key
         , Eq val
         ) => Algebra (KeyValStoreRead key val :+: KeyValStoreWrite key val :+: sig) (InMemoryListKeyValStoreC key val m) where
  alg (L (KVGet key k)) = (k =<<) . InMemoryListKeyValStoreC $ do
    table <- sendM . readMVar . unLookupVar =<< ask @(MutableLookup key val)
    pure $ lookup key table
  alg (R (L (KVPut key val k))) = (k =<<) . InMemoryListKeyValStoreC $ do
    LookupVar lookupVar <- ask @(MutableLookup key val)
    sendM $ modifyMVar lookupVar $ \table ->
      pure $
        case lookup key table of
          Nothing         -> ((key, val) : table, KVPutOk val)
          Just val'
            | val' == val ->              (table, KVPutOk val')
            | otherwise   ->              (table, KVPutConflict val')
  alg (R (R other)) = InMemoryListKeyValStoreC (alg (R (handleCoercible other)))

newEmptyState :: Has (Lift IO) sig m
              => m (MutableLookup key val)
newEmptyState = sendM @IO $ LookupVar <$> newMVar []

runInMemoryListKeyValStore :: MutableLookup key val -> InMemoryListKeyValStoreC key val m b -> m b
runInMemoryListKeyValStore var
  = runReader var
  . runInMemoryListKeyValStoreC

runNewInMemoryListKeyValStore :: Has (Lift IO) sig m
                              => InMemoryListKeyValStoreC key val m b -> m b
runNewInMemoryListKeyValStore m = do
  var <- newEmptyState
  runInMemoryListKeyValStore var m
