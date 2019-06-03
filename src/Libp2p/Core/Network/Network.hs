{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE AllowAmbiguousTypes#-}
module Libp2p.Core.Network.Network (
) where
import qualified Data.Map as M
import qualified Libp2p.Core.Peer.Peer as P
import qualified Libp2p.Core.PeerStore as PS

class (PS.Peerstore b) => Dialer a b c where
  peerstore::a -> m b
  localPeer:: a -> m P.ID
  closePeer:: a -> P.ID -> m ()
  peers:: a -> m [P.ID]
  dialPeer::a -> P.ID -> m (Either String c)
  connectedness::a -> P.ID -> m Int
  conns:: a -> m [b]
  connsToPeer::a -> P.ID -> m [b]
  

