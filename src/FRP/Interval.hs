{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FRP.Interval
    ( Interval
    , suspendInterval
    , defer
    ) where

import           Control.Applicative
import           Control.Monad.Writer.Strict

import           FRP.Managed

newtype Action m = Action (m () -> m ())

instance Monoid (Action m) where
    mempty = Action id
    Action f `mappend` Action g = Action (f . g)

action :: Monad m => m () -> Action m
action m = Action (m >>)

runAction :: Monad m => Action m -> m ()
runAction (Action f) = f (return ())

newtype Interval a = Interval { runInterval :: WriterT (Action Managed) IO a }
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

suspendInterval :: Interval a -> IO (a, IO ())
suspendInterval m = do
    (a, s) <- runWriterT (runInterval m)
    (_, d) <- suspend (runAction s)
    return (a, d)

defer :: Managed () -> Interval ()
defer = Interval . tell . action
