{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE AllowAmbiguousTypes #-}
module Libp2p.Core.Network.Internal where

import           Libp2p.Core.Peer.Peer
import qualified Libp2p.Core.Crypto as CY   
import qualified Libp2p.Multiaddr  as MA
import qualified Libp2p.Core.Mux as MUX
import qualified Libp2p.Core.Protocol.Switch as SW
import qualified Data.Map as M

data Stat = Stat {
    direction::Int,
    extra::M.Map String String
}

class (ConnSecurity a,ConnMultiaddrs a,MUX.Closer a) => Conn a b where
  newStream:: a -> m (Either String b)
  getStreams::a -> m [b]
  connStat::a -> m Stat

class ConnSecurity a where
  localPeer:: a -> m ID
  localPrivateKey::a -> m CY.PrivateKey
  remotePeer:: a -> m ID
  remotePublicKey:: a -> m CY.PubKey

class ConnMultiaddrs a where
  localMultiaddr:: a -> m [MA.Multiaddr]
  remoteMultiaddr::a -> m [MA.Multiaddr]

class (MUX.MuxedStream a) => Stream a b where
   protocol::a -> m SW.ID
   setProtocol:: a -> SW.ID -> m a
   streamStat:: a -> m Stat
   conn:: a -> m b