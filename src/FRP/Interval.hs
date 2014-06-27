{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FRP.Interval
    ( Dispose
    , dispose
    , Interval(..)
    , runInterval
    , atEnd
    ) where

import           Control.Applicative
import           Control.Monad.Writer.Strict

newtype Dispose = Dispose { runDispose :: IO () -> IO () }

instance Monoid Dispose where
    mempty                        = Dispose id
    Dispose f `mappend` Dispose g = Dispose (f . g)

dispose :: Dispose -> IO ()
dispose d = runDispose d (return ())

newtype Interval a = Interval { unInterval :: WriterT Dispose IO a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadWriter Dispose)

runInterval :: Interval a -> IO (a, Dispose)
runInterval = runWriterT . unInterval

atEnd :: IO () -> Interval ()
atEnd m = tell $ Dispose (m >>)
