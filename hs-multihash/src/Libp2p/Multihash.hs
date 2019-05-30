{-# language InstanceSigs #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleInstances #-}
{-# language RecordWildCards#-}
{-# language DataKinds #-}
module Libp2p.Multihash (
 MultihashType(..),
 Multihash(..),
 fromBS58String,
 toByteString,
 toBS58String,
 toHexString,
 fromHexString,
 fromByteString,
 sumHash
) where
import Data.Maybe
import Text.Read
import Data.ByteString as BS
import Data.ByteString.Char8 as C8
import Data.ByteString.Lazy as BSL
import Data.Bytes.VarInt
import Data.Bytes.Put
import Data.Bytes.Serial
import Control.Monad
import Data.Either
import Data.Text
import Data.ByteString.Base58 as BS58
import Data.Serialize as DS
import Data.HexString as Hex
import qualified Crypto.Hash as CH
import qualified Data.ByteArray as BA
import Data.Word

data MultihashType = ID | SHA1  | SHA2_256 | SHA2_512 | SHA3_224 | SHA3_256 | SHA3_384 | SHA3_512 | SHA3 | KECCAK_224 | KECCAK_256 |
                     KECCAK_384 | KECCAK_512 | SHAKE_128 | SHAKE_256 | BLAKE2B_MIN | BLAKE2B_MAX | BLAKE2S_MIN | BLAKE2S_MAX | MD5
                     deriving (Eq)

mCodeTable = [(ID, 0x00),(SHA1, 0x11),(SHA2_256, 0x12),(SHA2_512, 0x13),(SHA3_224, 0x17),(SHA3_256, 0x16),(SHA3_384, 0x15),(SHA3_512, 0x14),
              (SHA3,  0x14),(KECCAK_224, 0x1A),(KECCAK_256, 0x1B),(KECCAK_384, 0x1C),(KECCAK_512, 0x1D),(SHAKE_128,0x18) ,(SHAKE_256, 0x19),(BLAKE2B_MIN, 0xb201),
              (BLAKE2B_MAX, 0xb240),(BLAKE2S_MIN, 0xb241),(BLAKE2S_MAX, 0xb260),(MD5, 0xd5)]

hashTypeCode::MultihashType -> Maybe Int
hashTypeCode typ = lookup typ mCodeTable

hashTypeFromCode::Int -> Maybe MultihashType
hashTypeFromCode num = lookup num $ Prelude.map (\(a,b) -> (b,a)) mCodeTable

instance Show MultihashType where
   show  ID           = "id"
   show  SHA1         = "sha1"
   show  SHA2_256     = "sha2-256"
   show  SHA2_512     = "sha2-512"
   show  SHA3_224     = "sha3-224"
   show  SHA3_256     = "sha3-256"
   show  SHA3_384     = "sha3-384"
   show  SHA3_512     = "sha3-512"
   show  SHA3         = "sha3"
   show  KECCAK_224   = "keccak-224"
   show  KECCAK_256   = "keccak-256"
   show  KECCAK_384   = "keccak-384"
   show  KECCAK_512   = "keccak-512"
   show  SHAKE_128    = "shake-128"
   show  SHAKE_256    = "shake-256"
   show  BLAKE2B_MIN  = "blake2b-min"
   show  BLAKE2B_MAX  = "blake2b-max"
   show  BLAKE2S_MIN  = "blake2s-min"
   show  BLAKE2S_MAX  = "blake2b-max"
   show  MD5          = "md5"
 

instance Read MultihashType where
  readsPrec _ "id"           = [(ID,"")]
  readsPrec _ "sha1"         = [(SHA1,"")]
  readsPrec _ "sha2-512"     = [(SHA2_512,"")]
  readsPrec _ "sha3-224"     = [(SHA3_224,"")]
  readsPrec _ "sha3-256"     = [(SHA3_256,"")]
  readsPrec _ "sha3-384"     = [(SHA3_384,"")]
  readsPrec _ "sha3-512"     = [(SHA3_512,"")]
  readsPrec _ "sha3"         = [(SHA3,"")]
  readsPrec _ "keccak-224"   = [(KECCAK_224,"")]
  readsPrec _ "keccak-256"   = [(KECCAK_256,"")]
  readsPrec _ "keccak-384"   = [(KECCAK_384,"")]
  readsPrec _ "keccak-512"   = [(KECCAK_512,"")]
  readsPrec _ "shake-128"    = [(SHAKE_128,"")]
  readsPrec _ "shake-256"    = [(SHAKE_256,"")]
  readsPrec _ "blake2b-min"  = [(BLAKE2B_MIN,"")]
  readsPrec _ "blake2b-max"  = [(BLAKE2B_MAX,"")]
  readsPrec _ "blake2s-min"  = [(BLAKE2S_MIN,"")]
  readsPrec _ "blake2s-max"  = [(BLAKE2S_MAX,"")]
  readsPrec _ "md5"          = [(MD5,"")]
  readsPrec _ _              = []

defaultLength::MultihashType -> Int
defaultLength SHA1         = 20
defaultLength SHA2_256     = 32
defaultLength SHA2_512     = 64
defaultLength SHA3_224     = 28
defaultLength SHA3_256     = 32
defaultLength SHA3_384     = 48
defaultLength SHA3_512     = 64
defaultLength KECCAK_224   = 28
defaultLength KECCAK_256   = 32
defaultLength KECCAK_384   = 48
defaultLength KECCAK_512   = 64
defaultLength SHAKE_128    = 32
defaultLength SHAKE_256    = 64
defaultLength MD5          = 16
defaultLength  _           = -1

data Multihash = Multihash { hashType::MultihashType, digest::BS.ByteString } deriving (Eq)

instance DS.Serialize Multihash where
  put::Multihash -> Put
  put hash@Multihash{..} = do
    let code = VarInt $ fromJust $ hashTypeCode hashType
    serialize code
    let len = VarInt $ defaultLength hashType
    serialize len
    DS.putByteString digest
  get::Get Multihash
  get  = do
    (code::VarInt Int) <- deserialize
    let mayType = hashTypeFromCode (unVarInt code)
    when (isNothing mayType) (fail $ "multihash unknown function code: " <> show code)
    (varLen::VarInt Int) <- deserialize
    bytes <- getByteString (unVarInt varLen)
    pure $ Multihash (fromJust mayType) bytes

instance Read Multihash where
  readsPrec _ hash = case fromBS58String $ C8.pack hash of
                    Left  _ -> []
                    Right r -> [(r,"")]

instance Show Multihash where
  show  hash = C8.unpack $ toBS58String hash

fromByteString::BS.ByteString -> Either String Multihash
fromByteString = decode

fromBS58String::BS.ByteString -> Either String Multihash
fromBS58String str = case BS58.decodeBase58 BS58.bitcoinAlphabet str of
                       Nothing    -> Left "base58 fail"
                       Just bytes -> fromByteString bytes

toByteString::Multihash -> BS.ByteString
toByteString = encode

toBS58String::Multihash -> BS.ByteString
toBS58String hash = BS58.encodeBase58 BS58.bitcoinAlphabet (encode hash)

toHexString::Multihash -> Hex.HexString
toHexString hash = Hex.fromBytes $ encode hash

fromHexString::Hex.HexString -> Either String Multihash
fromHexString hex = decode $ Hex.toBytes hex


sumHash::BS.ByteString -> MultihashType -> Int -> BS.ByteString
sumHash bs typ len = let sliceLen = if len < 0 then defaultLength typ else len  
                     in  BA.pack $ Prelude.take len $ BA.unpack (CH.hash bs ::CH.Digest CH.MD5)

sumHash'::BS.ByteString -> MultihashType -> [Word8]
sumHash' bs ID          = BS.unpack bs
sumHash' bs SHA1        = BA.unpack (CH.hash bs ::CH.Digest CH.SHA1)
sumHash' bs SHA2_256    = BA.unpack (CH.hash bs ::CH.Digest CH.SHA256)
sumHash' bs SHA2_512    = BA.unpack (CH.hash bs ::CH.Digest CH.SHA512)
sumHash' bs SHA3_224    = BA.unpack (CH.hash bs ::CH.Digest CH.SHA3_224)
sumHash' bs SHA3_256    = BA.unpack (CH.hash bs ::CH.Digest CH.SHA3_256)
sumHash' bs SHA3_384    = BA.unpack (CH.hash bs ::CH.Digest CH.SHA3_384)
sumHash' bs SHA3_512    = BA.unpack (CH.hash bs ::CH.Digest CH.SHA3_512)
sumHash' bs SHA3        = BA.unpack (CH.hash bs ::CH.Digest CH.SHA384)
sumHash' bs KECCAK_224  = BA.unpack (CH.hash bs ::CH.Digest CH.Keccak_224)
sumHash' bs KECCAK_256  = BA.unpack (CH.hash bs ::CH.Digest CH.Keccak_256)
sumHash' bs KECCAK_384  = BA.unpack (CH.hash bs ::CH.Digest CH.Keccak_384)
sumHash' bs KECCAK_512  = BA.unpack (CH.hash bs ::CH.Digest CH.Keccak_512)
sumHash' bs SHAKE_128   = BA.unpack (CH.hash bs ::CH.Digest (CH.SHAKE128 128))
sumHash' bs SHAKE_256   = BA.unpack (CH.hash bs ::CH.Digest (CH.SHAKE256 256))
sumHash' bs BLAKE2B_MIN = BA.unpack (CH.hash bs ::CH.Digest CH.Blake2b_160)
sumHash' bs BLAKE2B_MAX = BA.unpack (CH.hash bs ::CH.Digest CH.Blake2b_512)
sumHash' bs BLAKE2S_MIN = BA.unpack (CH.hash bs ::CH.Digest CH.Blake2s_160)
sumHash' bs BLAKE2S_MAX = BA.unpack (CH.hash bs ::CH.Digest CH.Blake2s_256)
sumHash' bs MD5         = BA.unpack (CH.hash bs ::CH.Digest CH.MD5)