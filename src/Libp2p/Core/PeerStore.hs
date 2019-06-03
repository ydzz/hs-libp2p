module Libp2p.Core.PeerStore where
import           Libp2p.Core.Peer.Peer
import           Libp2p.Core.Peer.AddrInfo
import qualified Libp2p.Core.Crypto as CY    
import qualified Libp2p.Multiaddr  as MA

addressTTL :: Int
addressTTL = 60 * 60 * 1000

tempAddrTTL :: Int
tempAddrTTL = 2 * 60 * 1000

providerAddrTTL :: Int
providerAddrTTL = 10 * 60 * 1000

recentlyConnectedAddrTTL :: Int
recentlyConnectedAddrTTL = 10 * 60 * 1000

ownObservedAddrTTL :: Int
ownObservedAddrTTL = 10 * 60 * 1000

class (AddrBook a,KeyBook a,PeerMetadata a,Metrics a,ProtoBook a) => Peerstore a where
  peers::a -> [ID]
  peerInfo::a -> ID -> AddrInfo

class PeerMetadata a where
  get:: a -> ID -> String -> m String
  put:: a -> ID -> String -> String -> m a

class AddrBook a where
  addAddr :: a -> ID -> MA.Multiaddr   -> Int -> m a
  addAddrs:: a -> ID -> [MA.Multiaddr] -> Int -> m a
  setAddr::a -> ID -> MA.Multiaddr -> Int -> m a
  setAddrs::a -> ID -> [MA.Multiaddr] -> Int -> m a
  updateAddrs::a -> ID -> Int -> Int -> m a
  addrs:: a -> ID -> [MA.Multiaddr]
  clearAddrs::a -> ID -> m a
  peersWithAddrs::a -> m [ID]

class KeyBook a where
  pubKey::a -> ID -> m CY.PubKey
  addPubKey:: a -> ID -> CY.PubKey -> m a
  privKey::a -> ID -> m CY.PrivateKey
  addPrivKey::a -> ID -> CY.PrivateKey -> m a
  peersWithKeys::a -> m [ID]

class Metrics a where
  recordLatency:: a -> ID -> Int -> m a
  latencyEWMA:: a -> ID -> m 
  
class ProtoBook a where
  getProtocols::a -> ID -> m (Either String [String])
  addProtocols::a -> ID -> [String] -> m (Maybe String)
  setProtocols::a -> ID -> [String] -> m (Maybe String)
  supportsProtocols::a -> ID -> [String] -> m (Either String [String])