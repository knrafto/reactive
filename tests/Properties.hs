module Main ( main ) where

import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           FRP.LinkedList          as LinkedList

linkedList :: TestTree
linkedList = testGroup "linked list"
    [ testProperty "empty"  propEmpty
    , testProperty "insert" propInsert
    , testProperty "delete" propDelete
    , testProperty "clear"  propClear
    ]

insertAll :: [a] -> LinkedList a -> STM ()
insertAll xs ll = mapM_ (flip LinkedList.insert ll) xs

propEmpty :: Property
propEmpty = monadicIO $ do
    xs <- liftIO . atomically $ do
        ll <- LinkedList.empty
        LinkedList.toList ll
    assert $ null (xs :: [Int])

propInsert :: Property
propInsert = monadicIO $ do
    xs <- pick arbitrary :: PropertyM IO [Int]
    ys <- liftIO . atomically $ do
        ll <- LinkedList.empty
        insertAll xs ll
        LinkedList.toList ll
    assert $ ys == xs

propDelete :: Property
propDelete = monadicIO $ do
    xs <- pick arbitrary :: PropertyM IO [Int]
    ys <- pick arbitrary :: PropertyM IO [Int]
    a  <- pick arbitrary :: PropertyM IO Int
    zs <- liftIO . atomically $ do
        ll <- LinkedList.empty
        insertAll xs ll
        n <- LinkedList.insert a ll
        insertAll ys ll
        LinkedList.delete n
        LinkedList.toList ll
    assert $ zs == xs ++ ys

propClear :: Property
propClear = monadicIO $ do
    xs <- pick arbitrary :: PropertyM IO [Int]
    ys <- liftIO . atomically $ do
        ll <- LinkedList.empty
        insertAll xs ll
        LinkedList.clear ll
        LinkedList.toList ll
    assert $ null ys

tests :: TestTree
tests = testGroup "tests"
    [ linkedList
    ]

main :: IO ()
main = defaultMain tests
