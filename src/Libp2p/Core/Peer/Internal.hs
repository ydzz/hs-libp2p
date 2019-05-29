{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables#-}
{-# Language InstanceSigs#-}
module Libp2p.Core.Peer.Internal
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
import qualified Libp2p.Multihash as MH
import qualified Libp2p.Core.Crypto.Key as K
import Data.Either
import Data.Serialize as DS
import Data.HexString as Hex

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

fromPublicKey::(K.PubKey a) => a -> ID
fromPublicKey k = undefined

data AddrInfo = AddrInfo { peerId::ID
                         , addrs ::[MH.Multihash]
                         }
