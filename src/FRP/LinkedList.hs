-- A concurrent mutable doubly-linked list implementation.
module FRP.LinkedList
    ( -- * Types
      LinkedList
    , Node
      -- * Operations
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

-- | A mutable linked list.
newtype LinkedList a = LinkedList { listHead :: Node a }

-- | A handle to a position in the linked list. Two nodes are equal if and
-- only if they were created with the same 'insert' operation.
data Node a = Node
    { value :: Maybe a
    , prev  :: TVar (Node a)
    , next  :: TVar (Node a)
    }

instance Eq (Node a) where
    a == b = next a == next b

-- | Construct a new node.
newNode :: Maybe a -> Node a -> Node a -> STM (Node a)
newNode v p n = Node v <$> newTVar p <*> newTVar n

-- | /O(1)/. Contruct an empty linked list.
empty :: STM (LinkedList a)
empty = fmap LinkedList . mfix $ \n -> newNode Nothing n n

-- | /O(1)/. Insert an item at the end of a linked list, and return a handle
-- to its position.
insert :: a -> LinkedList a -> STM (Node a)
insert a ll = do
    let o = listHead ll
    m <- readTVar (prev o)
    n <- newNode (Just a) m o
    writeTVar (prev o) n
    writeTVar (next m) n
    return n

-- | /O(1)/. Delete a node from the linked list. Subsequent calls to 'delete'
-- on the same node will have no effect.
delete :: Node a -> STM ()
delete n = do
    m <- readTVar (prev n)
    o <- readTVar (next n)
    writeTVar (prev o) m
    writeTVar (next m) o
    writeTVar (prev n) n
    writeTVar (next n) n

-- | /O(1)/. Remove all nodes from the linked list.
clear :: LinkedList a -> STM ()
clear = delete . listHead

-- | /O(n)/. Return all items currently in the list.
toList :: LinkedList a -> STM [a]
toList = go [] . listHead
  where
    go acc n = do
        m <- readTVar (prev n)
        case value m of
            Nothing -> return acc
            Just a  -> go (a:acc) m
