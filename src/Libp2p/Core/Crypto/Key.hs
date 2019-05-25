{-# LANGUAGE MultiParamTypeClasses #-}
module Libp2p.Core.Crypto.Key where
import Data.ByteString


class Key a where
 bytes::a -> ByteString
 raw::  a -> ByteString
 typ::  a -> Int


class (Key a) => PubKey a where
  verify::a -> ByteString -> ByteString -> Bool

class (Key a) => PrivateKey a where
  pubKey::(PubKey b) => a -> b
  sign::a -> ByteString -> ByteString