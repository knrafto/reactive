{-# LANGUAGE RecursiveDo #-}
module Main ( main ) where

import           Control.Applicative
import           Control.Monad.STM
import           Control.Monad.Trans
import           Data.IORef
import           Data.Maybe
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           FRP.Interval
import           FRP.LinkedList          (LinkedList)
import qualified FRP.LinkedList          as LinkedList
import           FRP.Reactive

linkedList :: TestTree
linkedList = testGroup "linked list"
    [ testProperty "empty"  propListEmpty
    , testProperty "insert" propListInsert
    , testProperty "delete" propListDelete
    , testProperty "clear"  propListClear
    ]

insertAll :: [a] -> LinkedList a -> STM ()
insertAll xs ll = mapM_ (`LinkedList.insert` ll) xs

propListEmpty :: Property
propListEmpty = monadicIO $ do
    xs <- liftIO . atomically $ do
        ll <- LinkedList.empty
        LinkedList.toList ll
    assert $ null (xs :: [Int])

propListInsert :: Property
propListInsert = monadicIO $ do
    xs <- pick arbitrary
    ys <- liftIO . atomically $ do
        ll <- LinkedList.empty
        insertAll xs ll
        LinkedList.toList ll
    assert $ (ys :: [Int]) == xs

propListDelete :: Property
propListDelete = monadicIO $ do
    xs <- pick arbitrary
    ys <- pick arbitrary
    a  <- pick arbitrary
    zs <- liftIO . atomically $ do
        ll <- LinkedList.empty
        insertAll xs ll
        n <- LinkedList.insert a ll
        insertAll ys ll
        LinkedList.delete n
        LinkedList.toList ll
    assert $ (zs :: [Int]) == xs ++ ys

propListClear :: Property
propListClear = monadicIO $ do
    xs <- pick arbitrary
    ys <- liftIO . atomically $ do
        ll <- LinkedList.empty
        insertAll xs ll
        LinkedList.clear ll
        LinkedList.toList ll
    assert $ null (ys :: [Int])

frp :: TestTree
frp = testGroup "FRP"
    [ testGroup "Event"
        [ testProperty "fmap"       propEventMap
        , testProperty "empty"      propEventEmpty
        , testProperty "<|>"        propEventAlt
        , testProperty "filterJust" propFilterJust
        ]
    , testGroup "Behavior" []
    , testGroup "Combinators"
        [ testProperty "accum" propAccum
        ]
    ]

sink :: Event a -> Interval (IO [a])
sink e = do
    r <- liftIO $ newIORef []
    on e $ modifyIORef r . (:)
    return $ reverse <$> readIORef r

accum :: a -> Event (a -> a) -> Interval (Event a)
accum a f = mdo
    let e = flip ($) <$> b <@> f
    b <- hold a e
    return e

propEventMap :: Property
propEventMap = monadicIO $ do
    xs <- pick arbitrary
    f  <- pick arbitrary
    ys <- liftIO $ do
        ((push, out), d) <- runInterval $ do
            (e, push) <- newEvent
            out <- sink (f <$> e)
            return (push, out)
        mapM_ push xs
        dispose d
        out
    assert $ (ys :: [Int]) == map f (xs :: [Int])

propEventEmpty :: Property
propEventEmpty = monadicIO $ do
    xs <- pick arbitrary
    (ys1, ys2) <- liftIO $ do
        ((push, out1, out2), d) <- runInterval $ do
            (e, push) <- newEvent
            out1 <- sink (empty <|> e)
            out2 <- sink (e <|> empty)
            return (push, out1, out2)
        mapM_ push xs
        dispose d
        (,) <$> out1 <*> out2
    assert $ (ys1 :: [Int]) == xs && (ys2 :: [Int]) == xs

propEventAlt :: Property
propEventAlt = monadicIO $ do
    xs <- pick arbitrary
    ys <- liftIO $ do
        ((pushL, pushR, out), d) <- runInterval $ do
            (e1, pushL) <- newEvent
            (e2, pushR) <- newEvent
            out <- sink (e1 <|> e2)
            return (pushL, pushR, out)
        mapM_ (either pushL pushR) xs
        dispose d
        out
    assert $ (ys :: [Int]) == map (either id id) xs

propFilterJust :: Property
propFilterJust = monadicIO $ do
    xs <- pick arbitrary
    ys <- liftIO $ do
        ((push, out), d) <- runInterval $ do
            (e, push) <- newEvent
            out <- sink (filterJust e)
            return (push, out)
        mapM_ push xs
        dispose d
        out
    assert $ (ys :: [Int]) == catMaybes xs

propAccum :: Property
propAccum = monadicIO $ do
    xs <- pick arbitrary
    ys <- liftIO $ do
        ((push, out), d) <- runInterval $ do
            (e, push) <- newEvent
            e' <- accum 0 ((+) <$> e)
            out <- sink e'
            return (push, out)
        mapM_ push xs
        dispose d
        out
    assert $ (ys :: [Int]) == tail (scanl (+) 0 xs)

tests :: TestTree
tests = testGroup "tests"
    [ linkedList
    , frp
    ]

main :: IO ()
main = defaultMain tests
