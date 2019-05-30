{-# LANGUAGE TypeSynonymInstances#-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE TypeFamilies #-}
module Libp2p.Core.Crypto.RSA (
  generateRSAKeyPair,
  pubBytes,
  pubTyp,
  pubRaw,
  verify,
  privateBytes,
  privateRaw,
  privateTyp,
  sign
) where
import qualified Crypto.PubKey.RSA as R
import Crypto.Random.Types
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.X509 as X
import qualified Crypto.Store.X509 as XS
import qualified Data.PEM as PM
import qualified Crypto.Store.PKCS8 as P8
import qualified Crypto.PubKey.RSA.PKCS15 as P15
import qualified Crypto.Hash.Algorithms as R
import qualified Crypto.PubKey.RSA.Types as R
import qualified Libp2p.Core.Crypto.PB.PublicKey as PB
import qualified Libp2p.Core.Crypto.PB.PrivateKey as PB
import qualified Libp2p.Core.Crypto.PB.KeyType as PB
import Text.ProtocolBuffers(messageGet,messagePut,Utf8(..),defaultValue)

generateRSAKeyPair::(MonadRandom m) => Int  -> m (R.PublicKey,R.PrivateKey)
generateRSAKeyPair bits = R.generate bits 0x10001

pubBytes::R.PublicKey -> BS.ByteString
pubBytes k = LBS.toStrict $ messagePut $ PB.PublicKey (toEnum $ pubTyp k) (LBS.fromStrict $ PM.pemWriteBS $ XS.pubKeyToPEM $ X.PubKeyRSA k)

pubTyp::R.PublicKey -> Int
pubTyp   k = fromEnum PB.RSA

pubRaw::R.PublicKey -> BS.ByteString
pubRaw k = PM.pemWriteBS $ XS.pubKeyToPEM $ X.PubKeyRSA k

verify::R.PublicKey -> BS.ByteString -> BS.ByteString -> Bool
verify = P15.verify (Just R.SHA256)

privateBytes::R.PrivateKey -> BS.ByteString
privateBytes k = LBS.toStrict $ messagePut $ PB.PrivateKey (toEnum $ privateTyp k)
                                                           (LBS.fromStrict $ PM.pemWriteBS $ P8.keyToPEM P8.TraditionalFormat $ X.PrivKeyRSA k)

privateRaw::R.PrivateKey -> BS.ByteString
privateRaw k = PM.pemWriteBS $ P8.keyToPEM P8.TraditionalFormat $ X.PrivKeyRSA k 

privateTyp::R.PrivateKey -> Int
privateTyp k = fromEnum PB.RSA

sign::R.PrivateKey -> BS.ByteString -> Either R.Error BS.ByteString
sign = P15.sign Nothing (Just R.SHA256)