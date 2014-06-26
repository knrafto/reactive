{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Interval
    ( Dispose(..)
    , Interval(..)
    , runInterval
    , atEnd
    ) where

import           Control.Applicative
import           Control.Monad.Writer
import           Data.Monoid

newtype Dispose = Dispose { dispose :: IO () }

instance Monoid Dispose where
    mempty      = Dispose $ return ()
    mappend a b = Dispose $ dispose a >> dispose b

newtype Interval a = Interval { unInterval :: WriterT Dispose IO a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO, MonadWriter Dispose)

runInterval :: Interval a -> IO (a, Dispose)
runInterval = runWriterT . unInterval

atEnd :: Dispose -> Interval ()
atEnd = tell
