{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Sidechain.MerkleTree
  ( MerkleTree (..)
  , merkleHash
  , buildLeaf
  , buildBranch
  , getPath
  ) where


import Botan.Low.Hash (HashDigest, hashInit, hashUpdateFinalize)
import Data.ByteString.Builder (byteString, toLazyByteString, word64BE)
import Data.ByteString.Lazy (toStrict)
import Data.Word (Word64)
import Botan.Low.RNG (rngInit)
import Botan.Low.PubKey (PrivKey, PubKey, emsa_none, privKeyCreate, privKeyExportPubKey, pubKeyFingerprint)
import Botan.Low.PubKey.Sign
import Botan.Low.PubKey.Verify (verifyCreate, verifyFinish, verifyUpdate)
import Data.ByteString (ByteString)
import Debug.Trace
import GHC.IO (unsafePerformIO)

data MerkleTree a = Leaf a ByteString
                  | Branch ByteString (MerkleTree a)  (MerkleTree a)
    deriving (Show)

merkleHash :: ByteString -> IO ByteString
merkleHash bs = do
    h <- hashInit "SHA-256"
    hashUpdateFinalize h bs

buildLeaf :: ByteString -> IO (MerkleTree ByteString)
buildLeaf val = Leaf val <$> merkleHash val

buildBranch :: MerkleTree a -> MerkleTree a -> IO (MerkleTree a)
buildBranch l r = do
    h <- merkleHash (nodeHash l <> nodeHash r)
    pure (Branch h l r)
  where
    nodeHash (Leaf _ x)       = x
    nodeHash (Branch x _ _)   = x

createAccount :: IO (PrivKey, PubKey)
createAccount = do
    rng <- rngInit "system"
    priv <- privKeyCreate "ECDSA" "secp256k1" rng
    pub  <- privKeyExportPubKey priv          -- or privKeyGetField
    pure (priv, pub)

verify :: PubKey -> HashDigest -> ByteString -> IO Bool
verify pub digest signature = do
    verifier <- verifyCreate pub emsa_none StandardFormatSignature
    verifyUpdate verifier digest
    verifyFinish verifier signature

sign :: PrivKey -> HashDigest -> IO ByteString
sign priv digest = do
    rng <- rngInit "system"
    signer <- signCreate priv emsa_none StandardFormatSignature
    signUpdate signer digest
    signFinish signer rng

makeRawTx :: PubKey -> PubKey -> Word64 -> IO ByteString
makeRawTx from to amount = do
    fromAddr <- pubKeyFingerprint from "SHA-256"
    toAddr <- pubKeyFingerprint to "SHA-256"
    pure $ encodeRawTx fromAddr toAddr amount

encodeRawTx :: ByteString -> ByteString -> Word64 -> ByteString
encodeRawTx fromAddr toAddr amount =
    toStrict . toLazyByteString $
        byteString fromAddr
            <> byteString toAddr
            <> word64BE amount

createTransaction :: PrivKey -> ByteString -> IO (ByteString, ByteString)
createTransaction priv rawTx = do
    hash <- hashInit "SHA-256"
    txHash <- hashUpdateFinalize hash rawTx
    signature <- sign priv txHash
    pure (rawTx, signature)

verifyTx :: PubKey -> ByteString -> ByteString -> IO Bool
verifyTx pub rawTx signature = do
    hash <- hashInit "SHA-256"
    txHash <- hashUpdateFinalize hash rawTx
    verify pub txHash signature


exampleTree :: IO (MerkleTree ByteString)
exampleTree = do
    alice <- buildLeaf "alice"
    bob   <- buildLeaf "bob"
    carol <- buildLeaf "carol"
    dave  <- buildLeaf "dave"
    left  <- buildBranch alice bob
    right <- buildBranch carol dave
    buildBranch left right

type Pctxt a = (?tree :: MerkleTree a, ?path :: [(ByteString,ByteString)], ?start :: Bool)
getPath :: forall a. Eq a => a -> MerkleTree a -> [(ByteString,ByteString)]
getPath _ (Leaf _ _) = []
getPath item tree@(Branch _ rl rr) = let
  ?path = []
  ?start = True
  ?side = "root" 
  ?tree = tree
  in
  foldr @[] go output (setup tree)
  where
    go :: (ByteString, ByteString) -> (Pctxt a => [(ByteString, ByteString)]) -> (Pctxt a => [(ByteString, ByteString)])
    go n f = let ?path = n : ?path in f
    output :: (Pctxt a=> _)
    output = ?path
    setup :: (?side :: ByteString, ?tree :: MerkleTree a) =>  MerkleTree a -> [(ByteString,ByteString)]
    setup (Leaf _ m)
      | "left" <- ?side = [(m, "leafll")]
      | "right" <- ?side = [(m,"leafr")]
      | otherwise = error "invalid side for leaf"
    setup (Branch stem l r)
      | let contains (Branch x l r)
                = contains l || contains r
            contains (Leaf e _ )
                | e == item = True
                | otherwise = False
            isLeaf (Leaf _ _) = True
            isLeaf _ = False
            getHash (Leaf _ h) = h
            getHash (Branch h _ _) = h
      = if 
      | contains l
      , let ?side = "left"
      , not $ isLeaf ?tree
      , let ?tree = l
      -> (getHash r,"right") : setup l
      | contains l
      , let ?side = "left"
      , let ?tree = l
      -> (getHash l, "left") : setup l
      | contains r
      , let ?side = "right"
      , not $ isLeaf ?tree
      , let ?tree = r
      -> (getHash r,"left") : setup r
      | contains r
      , let ?side = "right"
      , let ?tree = r
      -> (getHash r,"right") : setup r
      | otherwise
      -> error "no element present"


validatePath :: ByteString -> [(ByteString,ByteString)] -> IO Bool
validatePath _  [] = return False
validatePath root p = let
  ?current = return "" in
  do  h <- setup p
      return (h == root)
  where
    isRight a = if a == "right" then "right" else error "no right node"
    isLeft  a = if a == "left" then "left" else error "no left node"
    setup :: (?current :: IO ByteString) => [(ByteString,ByteString)] -> IO ByteString
    setup [] = ?current 
    setup l@((y,"leafr"):(yy,(isLeft -> _)):ys)
       | let hash = do
               h <- hashInit "SHA-256"
               ih <- hashUpdateFinalize h (yy <> y)
               return ih
       = if 
       | let ?current = hash
       -> setup ys 
    setup l@((y,"leafll"):(yy,(isRight -> _)):ys)
       | let hash = do
               h <- hashInit "SHA-256"
               ih <- hashUpdateFinalize h (y <> yy)
               return ih
       = if 
       | let ?current = hash
       -> setup ys
    setup ((x,s):xs) =
      let hash = do    
            h <- hashInit "SHA-256"
            current <- ?current
            let value
                  | "right" <- s = (current <> x) 
                  | otherwise = (x <> current)
            ih <- hashUpdateFinalize h value
            return ih in
        let ?current = hash in traceShow (unsafePerformIO ?current) setup xs







{-

    go :: (ByteString)
      -> (Pctxt a=> [(ByteString,ByteString)])
      -> (Pctxt a=> [(ByteString,ByteString)])
    go "left" f
      | ?start
      , let ?start = False
      , let ?tree = rl
      = f
      | (Leaf _ hash) <- ?tree 
      , let ?path = (hash, "left") : ?path
      = f
      | (Branch _ x (Leaf _ u)) <- ?tree
      , let ?path = (u,"right") : ?path
      , let ?tree = x
      = f
      | (Branch _ x (Branch u _ _)) <- ?tree
      , let ?path = (u,"right") : ?path
      , let ?tree = x
      = f
    go "right" f
      | ?start
      , let ?start = False
      , let ?tree = rr   
      = f
      | (Leaf _ hash) <- ?tree
      , let ?path = (hash,"right") : ?path
      = f
      | (Branch _ (Leaf _ u) x) <- ?tree
      , let ?path = (u,"left") : ?path
      , let ?tree = x
      = f
      | (Branch _ (Branch u _ _) y) <- ?tree
      , let ?path = (u,"left") : ?path
      , let ?tree = y 
      = f
    go "leafll" f
      | (Leaf _ hash )<- ?tree
      , let ?path = (hash,"left") : ?path
      = f
    go "leafr" f
      | (Leaf _ hash )<- ?tree
      , let ?path = (hash,"right") : ?path
      = f
    go s _
      = error $ show s
-}
