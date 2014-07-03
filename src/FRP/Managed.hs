{-# LANGUAGE RankNTypes #-}
module FRP.Managed
    ( Managed(..)
    , bracket
    , finally
    , suspend
    ) where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception   as E
import           Control.Monad
import           Control.Monad.Trans

newtype Managed a = Managed { runManaged :: forall b. (a -> IO b) -> IO b }

instance Functor Managed where
    fmap f (Managed m) = Managed (\k -> m (k . f))

instance Applicative Managed where
    pure a = Managed (\k -> k a)
    (<*>)  = ap

instance Monad Managed where
    return a = Managed (\k -> k a)
    m >>= k  = Managed (\c -> runManaged m (\a -> runManaged (k a) c))

instance MonadIO Managed where
    liftIO m = Managed (m >>=)

bracket :: IO a -> (a -> IO ()) -> Managed a
bracket acquire release = Managed (E.bracket acquire release)

finally :: IO () -> Managed ()
finally release = Managed (\k -> k () `E.finally` release)

suspend :: Managed a -> IO (a, IO ())
suspend m = do
    result <- newEmptyMVar
    release <- newEmptyMVar
    _ <- forkIO . runManaged m $ \a -> do
        putMVar result a
        takeMVar release
    a <- takeMVar result
    return (a, putMVar release ())
