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
import           Control.Monad
import           Control.Monad.STM
import           Control.Monad.Trans
import           Data.IORef
import           Data.Monoid

import           FRP.Interval
import qualified FRP.LinkedList      as LinkedList

swapIORef :: IORef a -> a -> IO a
swapIORef r a = atomicModifyIORef' r $ \b -> (a, b)

data Behavior a = Behavior
    { sample :: IO a
    , listen :: (a -> IO ()) -> IO Dispose
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
        , listen = \_ -> return mempty
        , cached = True
        }

    b <*> c = Behavior
        { sample = sample b <*> sample c
        , listen = \k -> do
            d1 <- listen c $ \a -> do
                f <- sample b
                k (f a)
            d2 <- listen b $ \f -> do
                a <- sample c
                k (f a)
            return (d1 <> d2)
        , cached = False
        }

instance Monad Behavior where
    return = pure

    b >>= f = Behavior
        { sample = sample b >>= sample . f
        , listen = \k -> do
            clean <- liftIO $ newIORef mempty
            d1 <- listen b $ \a -> do
                let c = f a
                initial <- sample c
                k initial
                d' <- listen c k
                d <- swapIORef clean d'
                dispose d
            return . toDispose $ do
                dispose d1
                d2 <- readIORef clean
                dispose d2
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
    value <- liftIO $ newIORef initial
    ll    <- liftIO $ atomically LinkedList.empty
    let b = Behavior
            { sample = readIORef value
            , listen = \k -> do
                node <- atomically $ LinkedList.insert k ll
                return . toDispose . atomically $ LinkedList.delete node
            , cached = True
            }
        update !a = do
            writeIORef value a
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
    initial <- liftIO $ sample b
    liftIO $ f initial
    later $ listen b f

on :: Event a -> (a -> IO ()) -> Interval ()
on e f = do
    latch <- liftIO $ newIORef Nothing
    later $ listen (pulse e) $ \y -> do
        x <- swapIORef latch y
        case (x, y) of
            (Just a, Nothing) -> f a
            _                 -> return ()

class Cacheable f where
    cache :: f a -> Interval (f a)

instance Cacheable Event where
    cache e
        | cached (pulse e) = return e
        | otherwise        = do
            a           <- liftIO $ sample (pulse e)
            latch       <- liftIO $ newIORef a
            (b, update) <- newBehavior a
            later $ listen (pulse e) $ \y -> do
                x <- swapIORef latch y
                case (x, y) of
                    (Nothing, Nothing) -> return ()
                    _                  -> update y
            return (Event b)

instance Cacheable Behavior where
    cache b
        | cached b  = return b
        | otherwise = do
            a            <- liftIO $ sample b
            (b', update) <- newBehavior a
            later $ listen b update
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
