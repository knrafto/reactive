module Main ( main ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.STM
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
        , testProperty "mergeWith"  propMergeWith
        ]
    ]

jiffy :: MonadIO m => Interval a -> m a
jiffy m = liftIO $ do
    (a, d) <- runInterval m
    dispose d
    return a

sink :: Event a -> Interval (IO [a])
sink e = do
    r <- liftIO $ newIORef []
    on e $ modifyIORef r . (:)
    return $ reverse <$> readIORef r

propEventMap :: Property
propEventMap = monadicIO $ do
    xs <- pick arbitrary
    f  <- pick arbitrary
    ys <- jiffy $ do
        (e, push) <- newEvent
        out <- sink (f <$> e)
        liftIO $ do
            mapM_ push xs
            out
    assert $ (ys :: [Int]) == map f (xs :: [Int])

propEventEmpty :: Property
propEventEmpty = monadicIO $ do
    xs <- pick arbitrary
    (ys1, ys2) <- jiffy $ do
        (e, push) <- newEvent
        out1 <- sink (empty <|> e)
        out2 <- sink (e <|> empty)
        liftIO $ do
            mapM_ push xs
            (,) <$> out1 <*> out2
    assert $ (ys1 :: [Int]) == xs && (ys2 :: [Int]) == xs

propEventAlt :: Property
propEventAlt = monadicIO $ do
    xs <- pick arbitrary
    ys <- jiffy $ do
        (e1, pushL) <- newEvent
        (e2, pushR) <- newEvent
        out <- sink (e1 <|> e2)
        liftIO $ do
            mapM_ (either pushL pushR) xs
            out
    assert $ (ys :: [Int]) == map (either id id) xs

propFilterJust :: Property
propFilterJust = monadicIO $ do
    xs <- pick arbitrary
    ys <- jiffy $ do
        (e, push) <- newEvent
        out <- sink (filterJust e)
        liftIO $ do
            mapM_ push xs
            out
    assert $ (ys :: [Int]) == catMaybes xs

data Pick a = This a | That a | Both a
    deriving (Eq, Ord, Read, Show)

instance Arbitrary a => Arbitrary (Pick a) where
    arbitrary = oneof
        [This <$> arbitrary, That <$> arbitrary, Both <$> arbitrary]

collapsePick :: (a -> a -> a) -> Pick a -> a
collapsePick _ (This a) = a
collapsePick _ (That a) = a
collapsePick f (Both a) = f a a

propMergeWith :: Property
propMergeWith = monadicIO $ do
    xs <- pick arbitrary
    f  <- pick arbitrary
    ys <- jiffy $ do
        (e1, push1) <- newEvent
        (e2, push2) <- newEvent
        (e3, push3) <- newEvent
        out <- sink (mergeWith f (e1 <|> e3) (e2 <|> e3))
        liftIO $ do
            forM_ xs $ \x -> case x of
                This a -> push1 a
                That a -> push2 a
                Both a -> push3 a
            out
    assert $ (ys :: [Int]) == map (collapsePick f) xs

tests :: TestTree
tests = testGroup "tests"
    [ linkedList
    , frp
    ]

main :: IO ()
main = defaultMain tests
