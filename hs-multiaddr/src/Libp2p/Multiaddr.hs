{-# language InstanceSigs#-}
{-# language ScopedTypeVariables #-}
{-# language MultiWayIf #-}
module Libp2p.Multiaddr (
  Protocol(..),
  Multiaddr(..)
) where
import Control.Applicative (many, some, (<|>))
import Control.Monad (unless, void)
import qualified Data.IP                       as IP
import qualified Network.Socket                as Net
import qualified Data.ByteString               as BS
import qualified Libp2p.Multihash              as MH
import           Text.ParserCombinators.ReadP (ReadP, char, readP_to_S,readS_to_P, string)
import Data.Serialize
import Data.Bytes.VarInt
import Data.Bytes.Put
import Data.Int
import Data.Bytes.Serial
import Data.Maybe

data Protocol = IP4 IP.IPv4
              | TCP Net.PortNumber
              | UDP Net.PortNumber
              | DCCP Net.PortNumber
              | IP6 IP.IPv6
              | SCTP Net.PortNumber
              | IPFS MH.Multihash
              | P2P  MH.Multihash deriving (Eq)


protocolCodeMap = [("ip4",0x0004)
                  ,("tcp",0x0006)
                  ,("udp",0x0111)
                  ,("dccp",0x0021)
                  ,("ip6",0x0029)
                  ,("sctp",0x0029)
                  ,("ipfs",0x0029)
                  ,("p2p",0x0029)]

protocolCode'::String -> Int
protocolCode' k =  fromJust $ lookup k protocolCodeMap
protocolCode :: Protocol -> Int
protocolCode p = fromJust $ lookup (protocolName p) protocolCodeMap

protocolName::Protocol -> String
protocolName (IP4 _)  = "ip4"
protocolName (TCP _)  = "tcp"
protocolName (UDP _)  = "udp"
protocolName (DCCP _) = "dccp"
protocolName (IP6 _)  = "ip6"
protocolName (SCTP _) = "sctp"
protocolName (IPFS _) = "ipfs"
protocolName (P2P _)  = "p2p"

sep :: ReadP ()
sep = void $ some $ char '/'

protoString :: String -> ReadP ()
protoString s = sep *> void (string s)

readAddr :: Read a => ReadP a
readAddr = readS_to_P reads

parseAddr::(Read a) => (a -> Protocol) -> String -> ReadP Protocol
parseAddr c s = c <$> (protoString s *> sep *> readAddr)

instance Read Protocol where
    readsPrec _ = readP_to_S $ parseAddr IP4   "ip4"
                           <|> parseAddr TCP   "tcp"
                           <|> parseAddr UDP   "udp"
                           <|> parseAddr DCCP  "dccp"
                           <|> parseAddr IP6   "ip6"
                           <|> parseAddr SCTP  "sctp"
                           <|> parseAddr IPFS  "ipfs"
                           <|> parseAddr P2P   "p2p"

instance Show Protocol where
    show (IP4  p)   = "/ip4/"  ++ show p
    show (TCP  p)   = "/tcp/"  ++ show p
    show (UDP  p)   = "/udp/"  ++ show p
    show (DCCP p)   = "/dccp/" ++ show p
    show (IP6  p)   = "/ip6/"  ++ show p
    show (SCTP p)   = "/sctp/" ++ show p
    show (IPFS p)   = "/ipfs/" ++ show p
    show (P2P  p)   = "/ipfs/" ++ show p

newtype Multiaddr = Multiaddr { parts :: [Protocol] } deriving (Eq)

instance Read Multiaddr where
    readsPrec _ = readP_to_S $ do
        mparts <- some $ readS_to_P reads
        void $ many (char '/')
        return $ Multiaddr mparts

instance Show Multiaddr where
  show (Multiaddr m) = concatMap show m

instance Serialize IP.IPv4 where
    put v = putListOf putInt32be (map fromIntegral (IP.fromIPv4 v))
    get = IP.toIPv4 . map  fromIntegral <$> getListOf getInt32be

instance Serialize IP.IPv6 where
    put v = putListOf putInt32be (map fromIntegral (IP.fromIPv6 v))
    get = IP.toIPv6 . map  fromIntegral <$> getListOf getInt32be

instance Serialize Net.PortNumber where
    put w16 = putInt16be $ fromIntegral w16
    get     = fromIntegral <$> getInt16be

instance Serialize Protocol where
  put::Protocol -> Put
  put p = do
    let vCode = VarInt $ protocolCode p
    serialize vCode
    case p of
        IP4  v -> put v
        TCP  v -> put v
        UDP  v -> put v
        DCCP v -> put v
        IP6  v -> put v
        SCTP v -> put v
        IPFS v -> put v
        P2P  v -> put v
  get::Get Protocol
  get = do
    vCode::VarInt Int <- deserialize
    if | unVarInt vCode == protocolCode' "ip4"  -> IP4  <$> get
       | unVarInt vCode == protocolCode' "tcp"  -> TCP  <$> get
       | unVarInt vCode == protocolCode' "udp"  -> UDP  <$> get
       | unVarInt vCode == protocolCode' "dccp" -> DCCP <$> get
       | unVarInt vCode == protocolCode' "ip6"  -> IP6  <$> get
       | unVarInt vCode == protocolCode' "sctp" -> SCTP <$> get
       | unVarInt vCode == protocolCode' "ipfs" -> IPFS <$> get
       | unVarInt vCode == protocolCode' "p2p"  -> IP4  <$> get

instance Serialize Multiaddr where
  put::Multiaddr -> Put
  put (Multiaddr parts) = putListOf put parts
  get::Get Multiaddr
  get = do
     parts::[Protocol] <- getListOf get
     pure $ Multiaddr parts