module Yamux.Session where
import Data.Int
import Yamux.Config
import Network.Socket

data Session = Session {
    remoteGoAway::Int,
    localGoAway ::Int,
    nextStreamID::Int
}

newSession::Config -> Socket -> Bool -> Bool -> IO Session
newSession config sock isClient readBuf = return undefined