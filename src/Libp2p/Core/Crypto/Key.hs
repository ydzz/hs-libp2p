{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes#-}
{-# LANGUAGE TypeFamilies #-}
module Libp2p.Core.Crypto.Key (
  PrivateKey(..),
  PubKey(..),
  Key(..)
) where
import Data.ByteString
import qualified Crypto.PubKey.RSA.Types as R

class Key a where
 bytes::a -> ByteString
 raw::  a -> ByteString
 typ::  a -> Int


class (Key a) => PubKey a where
  verify::a -> ByteString -> ByteString -> Bool

class (Key a) => PrivateKey a where
  type GPubKeyType a :: *
  pubKey::a -> GPubKeyType a
  sign::a -> ByteString -> Either R.Error ByteString