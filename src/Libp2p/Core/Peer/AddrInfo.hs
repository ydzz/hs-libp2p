module Libp2p.Core.Peer.AddrInfo (
  AddrInfo(..)
) where
import qualified Libp2p.Multihash as MH
import qualified Libp2p.Multiaddr as MA
import Libp2p.Core.Peer.Peer
data AddrInfo = AddrInfo { peerId::ID
                         , addrs ::[MH.Multihash]
                         } deriving (Show)

--fromMultiaddr::MA.Multiaddr -> Either String AddrInfo
--fromMultiaddr maddr = if length (MA.parts maddr) < 1
--                      then Left "invalid p2p multiaddr"
--                      else Left "hh"