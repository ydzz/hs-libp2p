{-# LANGUAGE TypeSynonymInstances#-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE TypeFamilies #-}
module Libp2p.Core.Crypto.RSA (
  generateRSAKeyPair,
  RSAPrivateKey,
  RSAPublicKey
) where
import Libp2p.Core.Crypto.Key
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


generateRSAKeyPair::(MonadRandom m) => Int  -> m (RSAPublicKey,RSAPrivateKey)
generateRSAKeyPair bits = R.generate bits 0x10001

type RSAPublicKey  = R.PublicKey

instance Key RSAPublicKey where
  bytes k = LBS.toStrict $ messagePut $ PB.PublicKey (toEnum $ typ k)
                                                     (LBS.fromStrict $ PM.pemWriteBS $ XS.pubKeyToPEM $ X.PubKeyRSA k)
  raw   k = PM.pemWriteBS $ XS.pubKeyToPEM $ X.PubKeyRSA k
  typ   k = fromEnum PB.RSA

instance PubKey RSAPublicKey where
  verify = P15.verify (Just R.SHA256)

type RSAPrivateKey = R.PrivateKey

instance Key RSAPrivateKey where
  bytes k = LBS.toStrict $ messagePut $ PB.PrivateKey (toEnum $ typ k)
                                                      (LBS.fromStrict $ PM.pemWriteBS $ P8.keyToPEM P8.TraditionalFormat $ X.PrivKeyRSA k)
  raw   k = PM.pemWriteBS $ P8.keyToPEM P8.TraditionalFormat $ X.PrivKeyRSA k
  typ   k = fromEnum PB.RSA

instance PrivateKey RSAPrivateKey where
  type     GPubKeyType RSAPrivateKey =  RSAPublicKey
  pubKey  k =  R.private_pub k::RSAPublicKey
  sign     =  P15.sign Nothing (Just R.SHA256)