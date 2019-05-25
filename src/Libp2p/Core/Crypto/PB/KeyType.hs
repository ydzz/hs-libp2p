{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Libp2p.Core.Crypto.PB.KeyType (KeyType(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'

data KeyType = RSA
             | Ed25519
             | Secp256k1
             | ECDSA
               deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data,
                         Prelude'.Generic)

instance P'.Mergeable KeyType

instance Prelude'.Bounded KeyType where
  minBound = RSA
  maxBound = ECDSA

instance P'.Default KeyType where
  defaultValue = RSA

toMaybe'Enum :: Prelude'.Int -> P'.Maybe KeyType
toMaybe'Enum 0 = Prelude'.Just RSA
toMaybe'Enum 1 = Prelude'.Just Ed25519
toMaybe'Enum 2 = Prelude'.Just Secp256k1
toMaybe'Enum 3 = Prelude'.Just ECDSA
toMaybe'Enum _ = Prelude'.Nothing

instance Prelude'.Enum KeyType where
  fromEnum RSA = 0
  fromEnum Ed25519 = 1
  fromEnum Secp256k1 = 2
  fromEnum ECDSA = 3
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Libp2p.Core.Crypto.PB.KeyType") . toMaybe'Enum
  succ RSA = Ed25519
  succ Ed25519 = Secp256k1
  succ Secp256k1 = ECDSA
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Libp2p.Core.Crypto.PB.KeyType"
  pred Ed25519 = RSA
  pred Secp256k1 = Ed25519
  pred ECDSA = Secp256k1
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Libp2p.Core.Crypto.PB.KeyType"

instance P'.Wire KeyType where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'

instance P'.GPB KeyType

instance P'.MessageAPI msg' (msg' -> KeyType) KeyType where
  getVal m' f' = f' m'

instance P'.ReflectEnum KeyType where
  reflectEnum = [(0, "RSA", RSA), (1, "Ed25519", Ed25519), (2, "Secp256k1", Secp256k1), (3, "ECDSA", ECDSA)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Libp2p.Core.Crypto.PB.KeyType") [] ["Libp2p", "Core", "Crypto", "PB"] "KeyType")
      ["Libp2p", "Core", "Crypto", "PB", "KeyType.hs"]
      [(0, "RSA"), (1, "Ed25519"), (2, "Secp256k1"), (3, "ECDSA")]

instance P'.TextType KeyType where
  tellT = P'.tellShow
  getT = P'.getRead