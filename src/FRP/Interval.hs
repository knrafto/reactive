{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FRP.Interval
    ( Dispose(..)
    , dispose
    , Interval(..)
    , runInterval
    , atEnd
    ) where

import           Control.Applicative
import           Control.Monad.Writer.Strict

data Dispose
    = Empty
    | Dispose !(IO ())

instance Monoid Dispose where
    mempty = Empty

    Empty     `mappend` b         = b
    a         `mappend` Empty     = a
    Dispose a `mappend` Dispose b = Dispose (a >> b)

dispose :: Dispose -> IO ()
dispose Empty       = return ()
dispose (Dispose m) = m

newtype Interval a = Interval { unInterval :: WriterT Dispose IO a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadWriter Dispose)

runInterval :: Interval a -> IO (a, Dispose)
runInterval = runWriterT . unInterval

atEnd :: Dispose -> Interval ()
atEnd = tell
