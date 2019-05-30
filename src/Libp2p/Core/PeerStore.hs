module Libp2p.Core.PeerStore where
import Libp2p.Core.Peer.Peer
import qualified Libp2p.Multiaddr as MA

addressTTL::Int
addressTTL = 60 * 60 * 1000

tempAddrTTL::Int
tempAddrTTL = 2 * 60 * 1000

providerAddrTTL::Int
providerAddrTTL = 10 * 60 * 1000

recentlyConnectedAddrTTL::Int
recentlyConnectedAddrTTL = 10 * 60 * 1000

ownObservedAddrTTL::Int
ownObservedAddrTTL = 10 * 60 * 1000

class AddrBook a where
  addAddr :: a -> ID -> MA.Multiaddr   -> Int ->  m a
  addAddrs:: a -> ID -> [MA.Multiaddr]  -> Int -> m da
  addrs:: a -> ID -> [MA.Multiaddr]
     