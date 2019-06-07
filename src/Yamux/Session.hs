{-#LANGUAGE RecordWildCards#-}
{-#LANGUAGE ScopedTypeVariables#-}
module Yamux.Session where
import Data.Int
import Yamux.Config
import qualified Yamux.Const as YC
import Network.Socket
import qualified Network.Socket.ByteString as NSB
import System.Log.FastLogger
import qualified Data.ByteString as BS
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Network as ISN
import           System.IO
import Prelude
import Control.Monad
import Control.Concurrent
import qualified Data.Serialize as DS
import Data.Either

data Session = Session {
    remoteGoAway::Int,
    localGoAway ::Int,
    nextStreamID::Int,
    config::Config,
    logger::Maybe LoggerSet,
    conn::Socket,
    reader::S.InputStream BS.ByteString
}

newSession::Config -> Socket -> Bool -> Int -> IO Session
newSession config sock isClient readBuf = do
  (inS, _) <- if readBuf > 0
                then ISN.socketToStreamsWithBufferSize readBuf sock
                else ISN.socketToStreams sock
  mayLoggerSet <- newLogger
  let sess = Session {
    remoteGoAway = 0,
    localGoAway  = 0,
    nextStreamID = if isClient then 1 else 2,
    config = config,
    conn = sock,
    reader = inS,
    logger = mayLoggerSet
  }
  forkIO (sessionRecv sess)
  pure sess
 where
  newLogger::IO (Maybe LoggerSet)
  newLogger = case logOutput config of
                LogStdout size -> Just <$> newStdoutLoggerSet size
                LogStderr size -> Just <$> newStderrLoggerSet size
                _ -> pure Nothing

msgHandler::YC.MsgType -> (Session -> YC.Header -> IO ())
msgHandler YC.TypeData = handleStreamMessage
msgHandler YC.TypeWindowUpdate = handleStreamMessage
msgHandler YC.TypePing = handlePing
msgHandler YC.TypeGoAway = handleGoAway

handleStreamMessage::Session -> YC.Header -> IO ()
handleStreamMessage sess header = do
  putStrLn "handleStreamMessage"

handlePing::Session -> YC.Header -> IO ()
handlePing sess header = do
  putStrLn "handlePing"


handleGoAway::Session -> YC.Header -> IO ()
handleGoAway sess header = do
  putStrLn "handleGoAway"

sessionRecv::Session -> IO ()
sessionRecv sess@Session{..} = do
  bsData <- NSB.recv conn YC.headerSize
  if BS.length bsData == 0
  then logInfo sess "close connect"
  else do
    let eHeader::Either String YC.Header = DS.decode bsData
    case eHeader of
      Left err     -> logInfo sess err
      Right header@YC.Header{..} -> do
                        logInfo sess (show header)
                        if YC.protoVersion == headerVersion 
                        then do
                              msgHandler headerMsgType sess header
                              sessionRecv sess
                        else logInfo sess "version error"
  
 


logInfo::Session -> String -> IO ()
logInfo Session{..} msg = case logger of 
  Nothing  -> pure ()
  Just set -> pushLogStrLn set (toLogStr msg)