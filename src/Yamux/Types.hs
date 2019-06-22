module Yamux.Types (
  Session(..),
  Stream(..),
  StreamState(..)
) where
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.TBQueue
import Yamux.Config
import System.Log.FastLogger
import Network.Socket
import qualified Data.Map as M
import qualified Data.ByteString as BS

data StreamState = StreamInit | StreamSYNSent | StreamSYNReceived | StreamEstablished 
                  | StreamLocalClose |  StreamRemoteClose | StreamClosed | StreamReset

data Session = Session {
    remoteGoAway::Int,
    localGoAway ::TVar Bool,
    nextStreamID::Int,
    config::Config,
    logger::Maybe LoggerSet,
    conn::Socket,
    acceptCh::TMVar Stream,
    -- streamId 必须是奇数
    isClient::Bool,
    streams::TVar (M.Map Int Stream)
}

data Stream = Stream {
    recvWindow::Int,
    sendWindow::Int,
    streamId::Int,
    session::Session,
    state::StreamState,
    recvBuf::BS.ByteString,
    recvNotifyCh::TBQueue Bool,
    sendNotifyCh::TBQueue Bool
}