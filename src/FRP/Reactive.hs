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
            listen c $ \a -> do
                f <- atomically $ sample b
                k (f a)
            listen b $ \f -> do
                a <- atomically $ sample c
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
                let c = f a
                initial <- atomically $ sample c
                k initial
                (_, d') <- runInterval $ listen c k
                d <- atomically $ swapTVar clean d'
                dispose d
            atEnd $ do
                d <- atomically $ readTVar clean
                dispose d
        , cached = False
        }

newtype Event a = Event { pulse :: Behavior (Maybe a) }

instance Functor Event where
    fmap f = Event . fmap (fmap f) . pulse

instance Applicative Event where
    pure    = Event . pure . pure
    f <*> a = Event $ (<*>) <$> pulse f <*> pulse a

instance Alternative Event where
    empty   = Event $ pure empty
    a <|> b = Event $ (<|>) <$> pulse a <*> pulse b

instance Monad Event where
    return  = pure
    e >>= k = Event $ pulse e >>= \m -> case m of
        Nothing -> return Nothing
        Just a  -> pulse (k a)
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
                atEnd . atomically $ LinkedList.delete node
            , cached = True
            }
        update !a = do
            atomically $ writeTVar value a
            hs <- atomically $ LinkedList.toList ll
            mapM_ ($ a) hs
    atEnd . atomically $ LinkedList.clear ll
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

on :: Event a -> (a -> IO ()) -> Interval ()
on e f = do
    latch <- liftSTM $ newTVar Nothing
    listen (pulse e) $ \y -> do
        x <- atomically $ swapTVar latch y
        case (x, y) of
            (Just a, Nothing) -> f a
            _                 -> return ()

class Cacheable f where
    cache :: f a -> Interval (f a)

instance Cacheable Event where
    cache e
        | cached (pulse e) = return e
        | otherwise        = do
            a           <- liftSTM $ sample (pulse e)
            latch       <- liftSTM $ newTVar a
            (b, update) <- newBehavior a
            listen (pulse e) $ \y -> do
                x <- atomically $ swapTVar latch y
                case (x, y) of
                    (Nothing, Nothing) -> return ()
                    _                  -> update y
            return (Event b)

instance Cacheable Behavior where
    cache b
        | cached b  = return b
        | otherwise = do
            a            <- liftSTM $ sample b
            (b', update) <- newBehavior a
            listen b update
            return b'

filterJust :: Event (Maybe a) -> Event a
filterJust e = Event $ join <$> pulse e

hold :: a -> Event a -> Interval (Behavior a)
hold a e = do
    (b, update) <- newBehavior a
    on e update
    return b

switch :: Behavior (Event a) -> Event a
switch s = Event $ s >>= pulse

execute :: Event (IO a) -> Interval (Event a)
execute e = do
    (e', push) <- newEvent
    on e (>>= push)
    return e'

infixl 4 <@>, <@

(<@>) :: Behavior (a -> b) -> Event a -> Event b
b <@> e = Event $ fmap <$> b <*> pulse e

(<@) :: Behavior b -> Event a -> Event b
b <@ e = const <$> b <@> e
