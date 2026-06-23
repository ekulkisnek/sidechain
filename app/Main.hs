{-# LANGUAGE  PartialTypeSignatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE  NoMonomorphismRestriction #-}
{-# LANGUAGE ViewPatterns #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeApplications #-}
-- {-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main (main) where

import Botan.Low.Hash (HashDigest, hashInit, hashUpdateFinalize)
import Data.ByteString (ByteString)
import Debug.Trace


main :: IO ()
main = putStrLn "Hello, Haskell!"



data MerkleTree a = Leaf a ByteString
                  | Branch ByteString (MerkleTree a)  (MerkleTree a)
    deriving (Show)


{-

makeTransaction ls = foldr @[] go output (setup ls)
  where
    go n f = f 
    output = undefined
    setup [] = []
    setup ((prv,pb):(pr,pub):xs)
      | let makeTx a = makeRawTx pub pub 5
      = undefined


  ?start=True
  ?valid=return False
  ?stub=""
  ?root=""
  in
  foldr go output ( setup p)
  where
    go :: (PathCtxt => _) -> (PathCtxt =>_) -> (PathCtxt =>IO _)
    go n f
      | let ?valid = do
              a <- n x
              b <- ?valid
              return (a && b)
      = do
      valid <- ?valid
      continue <- f
      return if
        | valid -> valid
        | otherwise -> continue
    output = ?valid
    setup :: PathCtxt => _ -> [PathCtxt => _]
    setup [] = []
    setup (m:y:ms)
        | ?start
        , let ?root = m
        , let ?stub = y
        = setup ms
        | let validate a = do        
                hash <- hashInit "SHA-256"
                ih <- hashUpdateFinalize hash (?stub <> m)
                if | ih == ?root
                   , (a == ih)
                   -> return True
                   | otherwise
                   -> return False
        , let ?root = m
        , let ?stub = y
        = (\x -> validate x) : setup ms
    setup [x] = [const $ do
        hash <- hashInit "SHA-256"
        ih <- hashUpdateFinalize hash item
        return $ ih == x ]


-}
makeBlock = undefined

blockData = [0 ,1 ..]

mainLoop ls = let ?u = [] in
  foldr @[] go output (setup ls)
  where
    go = undefined
    output = undefined
    setup
      | let getBlock a = undefined
      , let addBlock a = undefined
      = undefined
