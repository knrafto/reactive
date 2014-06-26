{-# LANGUAGE BangPatterns #-}
-- TODO: 'execute'
module Reactive
    ( Behavior
    , Event
      -- * Combinators
    , mergeWith
    , filterJust
    , hold
    , (<@>)
    , (<@)
      -- * Interface
    , newBehavior
    , newEvent
    , animate
    , smooth
    , Cachable(..)
    ) where

import           Control.Applicative
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Maybe
import           Data.Monoid

import           Interval
import qualified LinkedList                  as LinkedList

liftSTM :: MonadIO m => STM a -> m a
liftSTM = liftIO . atomically

data Behavior a = Behavior
    { sample :: STM a
    , listen :: (a -> IO ()) -> Interval ()
    , cached :: Bool
    }

instance Functor Behavior where
    fmap f b = Behavior
        { sample = f <$> sample b
        , listen = \k -> listen b (k . f)
        , cached = False
        }

instance Applicative Behavior where
    pure a = Behavior
        { sample = return a
        , listen = \_ -> return ()
        , cached = True
        }

    b <*> c = Behavior
        { sample = sample b <*> sample c
        , listen = \k -> listen c $ \a -> do
            f <- atomically $ sample b
            k (f a)
        , cached = False
        }

instance Monad Behavior where
    return = pure

    b >>= f = Behavior
        { sample = sample b >>= sample . f
        , listen = \k -> do
            clean <- liftSTM $ newTVar mempty
            listen b $ \a -> do
                (_, d') <- runInterval $ listen (f a) k
                d <- atomically $ swapTVar clean d'
                dispose d
            atEnd . Dispose $ do
                d <- atomically $ swapTVar clean mempty
                dispose d
        , cached = False
        }

newtype Event a = Event { pulse :: Behavior (Maybe a) }

instance Functor Event where
    fmap f = Event . fmap (fmap f) . pulse

instance Applicative Event where
    pure    = Event . pure . pure
    f <*> a = Event ((<*>) <$> pulse f <*> pulse a)

instance Alternative Event where
    empty   = Event (pure empty)
    a <|> b = Event ((<|>) <$> pulse a <*> pulse b)

instance Monad Event where
    return  = pure
    e >>= k = Event $ pulse e >>= \m -> case m of
        Nothing -> return Nothing
        Just a  -> pulse (k a)
    fail _  = empty

instance MonadPlus Event where
    mzero = empty
    mplus = (<|>)

mergeWith :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeWith f a b = Event (combine <$> pulse a <*> pulse b)
  where
    combine Nothing m         = m
    combine m       Nothing   = m
    combine (Just x) (Just y) = Just (f x y)

filterJust :: Event (Maybe a) -> Event a
filterJust e = Event (join <$> pulse e)

hold :: a -> Event a -> Interval (Behavior a)
hold initial e = do
    latch     <- liftSTM $ sample (pulse e) >>= newTVar
    (b, push) <- newBehavior initial
    listen (pulse e) $ \y -> do
        x <- atomically $ swapTVar latch y
        case (x, y) of
            (Just a, Nothing) -> push a
            _                 -> return ()
    return b

switch :: Behavior (Event a) -> Event a
switch s = Event (s >>= pulse)

infixl 4 <@>, <@

(<@>) :: Behavior (a -> b) -> Event a -> Event b
b <@> e = Event (fmap <$> b <*> pulse e)

(<@) :: Behavior b -> Event a -> Event b
b <@ e = const <$> b <@> e

newBehavior :: a -> Interval (Behavior a, a -> IO ())
newBehavior initial = do
    value <- liftSTM $ newTVar initial
    ll    <- liftSTM LinkedList.empty
    let b = Behavior
            { sample = readTVar value
            , listen = \k -> do
                node <- liftSTM $ LinkedList.insert k ll
                atEnd . Dispose . atomically $ LinkedList.delete node
            , cached = True
            }
        push !a = do
            hs <- atomically $ do
                writeTVar value a
                LinkedList.toList ll
            mapM_ ($ a) hs
    atEnd . Dispose . atomically $ LinkedList.clear ll
    return (b, push)

newEvent :: Interval (Event a, a -> IO ())
newEvent = do
    (b, push) <- newBehavior Nothing
    return (Event b, \a -> push (Just a) >> push Nothing)

smooth :: (a -> a -> Bool) -> Behavior a -> Interval (Behavior a)
smooth p b = do
    initial    <- liftSTM $ sample b
    latch      <- liftSTM $ newTVar initial
    (b', push) <- newBehavior initial
    listen b $ \y -> do
        x <- atomically $ swapTVar latch y
        unless (p x y) $ push y
    return b'

animate :: Behavior a -> (a -> IO ()) -> Interval ()
animate b f = do
    initial <- liftSTM $ sample b
    liftIO $ f initial
    listen b f

on :: Event a -> (a -> IO ()) -> Interval ()
on e f = animate (pulse e) $ maybe (return ()) f

class Cachable f where
    cache :: f a -> Interval (f a)

instance Cachable Event where
    cache e = Event <$> smooth flat (pulse e)
      where
        flat Nothing Nothing = True
        flat _       _       = False

instance Cachable Behavior where
    cache b
        | cached b  = return b
        | otherwise = do
            initial    <- liftSTM $ sample b
            (b', push) <- newBehavior initial
            listen b push
            return b'
