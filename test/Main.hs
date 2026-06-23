module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM_)
import Data.ByteString (ByteString)
import Fixtures (exampleTree)
import Sidechain.MerkleTree
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

main :: IO ()
main = defaultMain tests

tests :: Test.Tasty.TestTree
tests = testGroup "MerkleTree"
  [ testGroup "exampleTree"
      [ testCase "builds a branch root" $ do
          tree <- exampleTree
          case tree of
            Branch _ _ _ -> pure ()
            _ -> assertFailure "expected branch root"
      , testCase "each leaf has a non-empty path" $ do
          tree <- exampleTree
          forM_ (["alice", "bob", "carol", "dave"] :: [ByteString]) $ \name ->
            assertBool (show name ++ " path should not be empty") $
              not (null (getPath name tree))
      , testCase "unknown item is not in tree" $ do
          tree <- exampleTree
          result <- try @SomeException (evaluate (getPath "eve" tree))
          case result of
            Left _  -> pure ()
            Right _ -> assertFailure "expected error for missing item"
      ]
  ]



buildLeaf :: ByteString -> IO (MerkleTree ByteString)
buildLeaf val = Leaf val <$> merkleHash val

buildBranch :: MerkleTree a -> MerkleTree a -> IO (MerkleTree a)
buildBranch l r = do
    h <- merkleHash (nodeHash l <> nodeHash r)
    pure (Branch h l r)
  where
    nodeHash (Leaf _ x)    = x
    nodeHash (Branch x _ _) = x
