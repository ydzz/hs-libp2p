module Libp2p.Config.Config where
import qualified Libp2p.Core.Crypto as CY
data Config k = Config {
  peerKey::k
}