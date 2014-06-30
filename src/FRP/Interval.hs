{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FRP.Interval
    ( Dispose
    , toDispose
    , dispose
    , Interval(..)
    , runInterval
    , later
    , atEnd
    ) where

import           Control.Applicative
import           Control.Monad.Writer.Strict

newtype Dispose = Dispose (IO () -> IO ())

instance Monoid Dispose where
    mempty = Dispose id
    Dispose f `mappend` Dispose g = Dispose (f . g)

toDispose :: IO () -> Dispose
toDispose m = Dispose (m >>)

dispose :: Dispose -> IO ()
dispose (Dispose f) = f (return ())

newtype Setup = Setup (IO Dispose -> IO Dispose)

instance Monoid Setup where
    mempty = Setup id
    Setup f `mappend` Setup g = Setup (f . g)

setup :: IO Dispose -> Setup
setup m = Setup (liftA2 mappend m)

runSetup :: Setup -> IO Dispose
runSetup (Setup f) = f (return mempty)

newtype Interval a = Interval (WriterT Setup IO a)
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

runInterval :: Interval a -> IO (a, Dispose)
runInterval (Interval m) = do
    (a, s) <- runWriterT m
    d <- runSetup s
    return (a, d)

later :: IO Dispose -> Interval ()
later = Interval . tell . setup

atEnd :: IO () -> Interval ()
atEnd = later . return . toDispose
