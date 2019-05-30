{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables#-}
{-# Language InstanceSigs#-}
{-# Language AllowAmbiguousTypes#-}
module Libp2p.Core.Peer.Peer
  ( pretty
  , ID(..)
  , fromBS58String
  , toHexString
  , fromHexString
  )
where
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Char8         as C8
import           Data.ByteString.Base58        as BS58
import qualified Libp2p.Core.Crypto as CY
import  Libp2p.Core.Crypto.RSA
import Data.Either
import Data.Serialize as DS
import Data.HexString as Hex
import qualified Libp2p.Multihash as MH

newtype ID = ID BS.ByteString

pretty :: ID -> BS.ByteString
pretty (ID bs) = BS58.encodeBase58 BS58.bitcoinAlphabet bs

instance Show ID where
  show pid = C8.unpack $ pretty pid

instance Read ID where
  readsPrec _  bs =  [(ID $ C8.pack bs,"") | isRight ( MH.fromByteString (C8.pack bs))]

instance DS.Serialize ID where
  put (ID bs) = DS.putByteString bs
  get = ID . MH.toByteString <$> DS.get

fromBS58String::BS.ByteString -> Either String ID
fromBS58String strBS58 = ID . MH.toByteString <$> MH.fromBS58String strBS58

toHexString::ID ->  Hex.HexString
toHexString (ID pid) = Hex.fromBytes pid

fromHexString::Hex.HexString -> Either String ID
fromHexString hexString = ID . MH.toByteString <$> MH.fromHexString hexString

advancedEnableInlining = True
maxInlineKeyLength     = 42

fromPublicKey::CY.PubKey -> ID
fromPublicKey k = if advancedEnableInlining && BS.length kBytes < maxInlineKeyLength
                  then ID kBytes
                  else ID sumHash
  where
    kBytes = CY.pubBytes k
    sumHash = MH.sumHash kBytes MH.SHA2_256 (-1)


fromPrivateKey::CY.PrivateKey  -> ID
fromPrivateKey k = fromPublicKey (CY.pubKey k) 

