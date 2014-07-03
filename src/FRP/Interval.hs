{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FRP.Interval
    ( Managed
    , runManaged
    , finally
    , Interval
    , suspend
    , defer
    ) where

import           Control.Applicative
import           Control.Monad.Writer.Strict

newtype Action m = Action (m () -> m ())

instance Monoid (Action m) where
    mempty = Action id
    Action f `mappend` Action g = Action (f . g)

action :: Monad m => m () -> Action m
action m = Action (m >>)

runAction :: Monad m => Action m -> m ()
runAction (Action f) = f (return ())

newtype Managed a = Managed (WriterT (Action IO) IO a)
    deriving (Functor, Applicative, Monad, MonadIO)

runManaged :: Managed a -> IO (a, IO ())
runManaged (Managed m) = do
    (a, d) <- runWriterT m
    return (a, runAction d)

finally :: IO () -> Managed ()
finally = Managed . tell . action

newtype Interval a = Interval (WriterT (Action Managed) IO a)
    deriving (Functor, Applicative, Monad, MonadFix, MonadIO)

suspend :: Interval a -> IO (a, IO ())
suspend (Interval m) = do
    (a, s) <- runWriterT m
    (_, d) <- runManaged (runAction s)
    return (a, d)

defer :: Managed () -> Interval ()
defer = Interval . tell . action
