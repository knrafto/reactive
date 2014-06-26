{-# LANGUAGE BangPatterns #-}
module FRP.Reactive
    ( Behavior
    , Event
      -- * Interface
    , newBehavior
    , newEvent
    , animate
    , on
    , Cacheable(..)
      -- * Combinators
    , mergeWith
    , filterJust
    , hold
    , switch
    , execute
    , (<@>)
    , (<@)
    ) where

import           Control.Applicative
import           Control.Concurrent.STM.TVar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Monoid

import           FRP.Interval
import qualified FRP.LinkedList               as LinkedList

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
        , listen = \k -> do
            listen b $ \f -> do
                a <- atomically $ sample c
                k (f a)
            listen c $ \a -> do
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
                d <- atomically $ readTVar clean
                dispose d
        , cached = False
        }

newtype Event a = Event { signal :: Behavior (Maybe a) }

instance Functor Event where
    fmap f = Event . fmap (fmap f) . signal

instance Applicative Event where
    pure    = Event . pure . pure
    f <*> a = Event $ (<*>) <$> signal f <*> signal a

instance Alternative Event where
    empty   = Event $ pure empty
    a <|> b = Event $ (<|>) <$> signal a <*> signal b

instance Monad Event where
    return  = pure
    e >>= k = Event $ signal e >>= \m -> case m of
        Nothing -> return Nothing
        Just a  -> signal (k a)
    fail _  = empty

instance MonadPlus Event where
    mzero = empty
    mplus = (<|>)

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
        update !a = do
            atomically $ writeTVar value a
            hs <- atomically $ LinkedList.toList ll
            mapM_ ($ a) hs
    atEnd . Dispose . atomically $ LinkedList.clear ll
    return (b, update)

newEvent :: Interval (Event a, a -> IO ())
newEvent = do
    (b, update) <- newBehavior Nothing
    return (Event b, \a -> update (Just a) >> update Nothing)

animate :: Behavior a -> (a -> IO ()) -> Interval ()
animate b f = do
    initial <- liftSTM $ sample b
    liftIO $ f initial
    listen b f

data Edge a = Up a | Down a

trigger :: Event a -> (Edge a -> IO ()) -> Interval ()
trigger e f = do
    latch <- liftSTM $ newTVar Nothing
    listen (signal e) $ \y -> do
        x <- atomically $ swapTVar latch y
        case (x, y) of
            (Nothing, Just a ) -> f (Up a)
            (Just a , Nothing) -> f (Down a)
            _                  -> return ()

on :: Event a -> (a -> IO ()) -> Interval ()
on e f = trigger e $ \r -> case r of
    Up a -> f a
    _    -> return ()

class Cacheable f where
    cache :: f a -> Interval (f a)

instance Cacheable Event where
    cache e
        | cached (signal e) = return e
        | otherwise         = do
            initial     <- liftSTM $ sample (signal e)
            (b, update) <- newBehavior initial
            trigger e $ \r -> case r of
                Up a -> update (Just a)
                _    -> update Nothing
            return (Event b)

instance Cacheable Behavior where
    cache b
        | cached b  = return b
        | otherwise = do
            initial      <- liftSTM $ sample b
            (b', update) <- newBehavior initial
            listen b update
            return b'

mergeWith :: (a -> a -> a) -> Event a -> Event a -> Event a
mergeWith f a b = Event $ combine <$> signal a <*> signal b
  where
    combine Nothing  m        = m
    combine m        Nothing  = m
    combine (Just x) (Just y) = Just (f x y)

filterJust :: Event (Maybe a) -> Event a
filterJust e = Event $ join <$> signal e

hold :: a -> Event a -> Interval (Behavior a)
hold initial e = do
    (b, update) <- newBehavior initial
    trigger e $ \r -> case r of
        Down a -> update a
        _      -> return ()
    return b

switch :: Behavior (Event a) -> Event a
switch s = Event $ s >>= signal

execute :: Event (IO a) -> Interval (Event a)
execute e = do
    (e', push) <- newEvent
    trigger e $ \r -> case r of
        Up m -> m >>= push
        _    -> return ()
    return e'

infixl 4 <@>, <@

(<@>) :: Behavior (a -> b) -> Event a -> Event b
b <@> e = Event Behavior
    { sample = fmap <$> sample b <*> sample (signal e)
    , listen = \k -> trigger e $ \r -> case r of
        Up a -> do
            f <- atomically $ sample b
            k (Just (f a))
        _    -> k Nothing
    , cached = False
    }

(<@) :: Behavior b -> Event a -> Event b
b <@ e = const <$> b <@> e
