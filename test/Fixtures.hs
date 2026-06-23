module Fixtures (exampleTree) where

import Data.ByteString (ByteString)
import Sidechain.MerkleTree

exampleTree :: IO (MerkleTree ByteString)
exampleTree = do
    alice <- buildLeaf "alice"
    bob   <- buildLeaf "bob"
    carol <- buildLeaf "carol"
    dave  <- buildLeaf "dave"
    left  <- buildBranch alice bob
    right <- buildBranch carol dave
    buildBranch left right
