module LinkedList
    ( LinkedList
    , Node
    , empty
    , insert
    , delete
    , clear
    , toList
    ) where

import           Control.Applicative         hiding (empty)
import           Control.Concurrent.STM.TVar
import           Control.Monad.Fix
import           Control.Monad.STM

newtype LinkedList a = LinkedList { listHead :: Node a }

data Node a = Node
    { value :: Maybe a
    , prev  :: TVar (Node a)
    , next  :: TVar (Node a)
    }

newNode :: Maybe a -> Node a -> Node a -> STM (Node a)
newNode v p n = Node v <$> newTVar p <*> newTVar n

empty :: STM (LinkedList a)
empty = fmap LinkedList . mfix $ \n -> newNode Nothing n n

insert :: a -> LinkedList a -> STM (Node a)
insert a ll = do
    let o = listHead ll
    m <- readTVar (prev o)
    n <- newNode (Just a) m o
    writeTVar (prev o) n
    writeTVar (next m) n
    return n

delete :: Node a -> STM ()
delete n = do
    m <- readTVar (prev n)
    o <- readTVar (next n)
    writeTVar (prev o) m
    writeTVar (next m) o
    writeTVar (prev n) n
    writeTVar (next n) n

clear :: LinkedList a -> STM ()
clear = delete . listHead

toList :: LinkedList a -> STM [a]
toList = go [] . listHead
  where
    go acc n = do
        m <- readTVar (prev n)
        case value m of
            Nothing -> return acc
            Just a  -> go (a:acc) m
