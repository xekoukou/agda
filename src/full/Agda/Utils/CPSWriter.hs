{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Agda.Utils.CPSWriter (
       CPSWriterT,
       CPSWriter,
       runCPSWriterT,
       runCPSWriter,
       execCPSWriterT,
       execCPSWriter,
       cpsWriterT) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import qualified Control.Monad.State.Class as MS
import Control.Monad.Reader
import Control.Monad.Trans.State.Strict
import Control.Monad.Writer.Class

newtype CPSWriterT w m a = CPSWriterT { outCPSWriterT :: StateT w m a }
type CPSWriter w = CPSWriterT w Identity

runCPSWriterT :: Monoid w => CPSWriterT w m a -> m (a, w)
runCPSWriterT tw = runStateT (outCPSWriterT tw) mempty

runCPSWriter :: Monoid w => CPSWriter w a -> (a, w)
runCPSWriter = runIdentity . runCPSWriterT

execCPSWriterT :: (Functor m, Monoid w) => CPSWriterT w m a -> m w
execCPSWriterT = fmap snd . runCPSWriterT

execCPSWriter :: Monoid w => CPSWriter w a -> w
execCPSWriter = snd . runCPSWriter

mapCPSWriterT :: (Monad n, Monoid w, Monoid w') => (m (a, w) -> n (b, w')) -> CPSWriterT w m a -> CPSWriterT w' n b
mapCPSWriterT f = cpsWriterT . f . runCPSWriterT

instance Functor m => Functor (CPSWriterT w m) where
  fmap f (CPSWriterT tw) = CPSWriterT $ fmap f tw

instance Monad m => Applicative (CPSWriterT w m) where
  pure     = CPSWriterT . pure
  f <*> tw = CPSWriterT $ outCPSWriterT f <*> outCPSWriterT tw

instance Monad m => Monad (CPSWriterT w m) where
  CPSWriterT m >>= f = CPSWriterT $ m >>= outCPSWriterT . f

instance MonadTrans (CPSWriterT w) where
  lift = CPSWriterT . lift 


cpsWriterT :: (Monoid w, Monad m) => m (a, w) -> CPSWriterT w m a
cpsWriterT maw = CPSWriterT $ do
              (a, w) <- lift maw
              w'     <- get
              let ww' = w `mappend` w'
              () <- ww' `seq` put ww'
              return a

instance (Monoid w, Monad m) => MonadWriter w (CPSWriterT w m) where
  writer = cpsWriterT . return
  listen ma = CPSWriterT $ outCPSWriterT ma  >>= \ a -> (a,) <$> get
  pass maf  = CPSWriterT $ outCPSWriterT maf >>= \ (a, f) -> modify f >> return a

instance MonadIO m => MonadIO (CPSWriterT w m) where
  liftIO = CPSWriterT . liftIO

instance MS.MonadState s m => MS.MonadState s (CPSWriterT w m) where
  state = CPSWriterT . lift . MS.state

instance (Monoid w, MonadReader r m) => MonadReader r (CPSWriterT w m) where
  ask   = lift ask
  local = mapCPSWriterT . local

instance MonadPlus m => MonadPlus (CPSWriterT w m) where
   mzero       = CPSWriterT mzero
   m `mplus` n = CPSWriterT (outCPSWriterT m `mplus` outCPSWriterT n)

instance MonadPlus m => Alternative (CPSWriterT w m) where
    empty = mzero
    (<|>) = mplus
