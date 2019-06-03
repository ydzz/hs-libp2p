module Yamux.Addr where
import Network.Socket

class HasAddr a where
    localAddr:: a -> SockAddr
    remoteAddr:: a -> SockAddr

newtype YamuxAddr = YamuxAddr { addr::String }

netWork::YamuxAddr -> String
netWork _ = "yamux"

instance Show YamuxAddr where
    show (YamuxAddr addr) = "yamux:" <> addr