{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes#-}
{-# LANGUAGE TypeFamilies #-}
module Libp2p.Core.Crypto (
  PubKey(..),
  PrivateKey(..),
  pubBytes,
  pubRaw,
  pubTyp,
  verify,
  privateBytes,
  privateRaw,
  privateTyp,
  sign,
  pubKey
) where
import Data.ByteString
import qualified Crypto.PubKey.RSA as CR
import qualified Libp2p.Core.Crypto.RSA as R

data PubKey  = RSAPubKey CR.PublicKey

pubBytes::PubKey -> ByteString
pubBytes (RSAPubKey k) = R.pubBytes k

pubRaw::PubKey -> ByteString
pubRaw (RSAPubKey k) = R.pubRaw k

pubTyp::PubKey -> Int
pubTyp (RSAPubKey k) = R.pubTyp k

verify::PubKey -> ByteString -> ByteString -> Bool
verify (RSAPubKey k) = R.verify k

data PrivateKey = RSAPrivateKey　CR.PrivateKey

privateBytes::PrivateKey -> ByteString
privateBytes (RSAPrivateKey k) = R.privateBytes k

privateRaw::PrivateKey -> ByteString
privateRaw (RSAPrivateKey k) = R.privateRaw k

privateTyp::PrivateKey -> Int
privateTyp (RSAPrivateKey k) = R.privateTyp k

sign ::PrivateKey -> ByteString -> Either CR.Error ByteString
sign (RSAPrivateKey k) = R.sign k

pubKey::PrivateKey -> PubKey
pubKey (RSAPrivateKey k) = RSAPubKey (CR.private_pub k)