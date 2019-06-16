{-#LANGUAGE RecordWildCards#-}
{-#LANGUAGE ScopedTypeVariables#-}
module Yamux.Session where
import Prelude
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
import Control.Monad
import qualified Control.Monad.Trans.Except as TE
import Control.Concurrent
import qualified Data.Serialize as DS
import Data.Either
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TMVar
import qualified Data.Map as M
import qualified Yamux.Stream as YS
import qualified Data.Bits as Bit
import Control.Monad.Trans.Class
import Control.Error.Safe
import Data.Serialize
import Yamux.Types
import Yamux.Stream

foreverExceptT::TE.ExceptT String IO () -> IO ()
foreverExceptT et = do
  e <- TE.runExceptT et
  case e of
    Left errString -> error errString
    Right _        -> foreverExceptT et

newSession::Config -> Socket -> Bool -> Int -> IO Session
newSession config sock isClient readBuf = do
  (inS, _) <- if readBuf > 0
                then ISN.socketToStreamsWithBufferSize readBuf sock
                else ISN.socketToStreams sock
  mayLoggerSet <- newLogger
  newStreams <- newTVarIO M.empty
  newLocalGoAway <- newTVarIO False
  emptyAcc <- atomically newEmptyTMVar
  let sess = Session {
    remoteGoAway = 0,
    localGoAway  = newLocalGoAway,
    nextStreamID = if isClient then 1 else 2,
    config = config,
    conn = sock,
    acceptCh = emptyAcc,
    logger = mayLoggerSet,
    streams = newStreams,
    isClient = isClient
  }
  foreverExceptT $ sessionRecv sess
  pure sess
 where
  newLogger::IO (Maybe LoggerSet)
  newLogger = case logOutput config of
                LogStdout size -> Just <$> newStdoutLoggerSet size
                LogStderr size -> Just <$> newStderrLoggerSet size
                _ -> pure Nothing


sessionRecv::Session -> TE.ExceptT String IO ()
sessionRecv sess@Session{..} =  do
  bsData <- lift $ NSB.recv conn YC.headerSize
  tryAssert "session closed" (BS.length bsData == 0)
  header <- TE.except (DS.decode bsData::Either String YC.Header)
  tryAssert "version error" (YC.protoVersion /= YC.headerVersion header)
  msgHandler (YC.headerMsgType header) sess header
  return ()


logInfo::Session -> String -> IO ()
logInfo Session{..} msg = case logger of
  Nothing  -> pure ()
  Just set -> pushLogStrLn set (toLogStr msg)


msgHandler::YC.MsgType -> (Session -> YC.Header -> TE.ExceptT String IO ())
msgHandler YC.TypeData = handleStreamMessage
msgHandler YC.TypeWindowUpdate = handleStreamMessage
msgHandler YC.TypePing = handlePing
msgHandler YC.TypeGoAway = handleGoAway

handleStreamMessage::Session -> YC.Header -> TE.ExceptT String IO ()
handleStreamMessage sess@Session{..} hdr@YC.Header{..} = do
  lift $ putStrLn "handleStreamMessage"
  if synFlag
     then incomingStream (fromIntegral headerStreamId)
     else do
      streamMap <- lift $ readTVarIO streams
      let curStream = M.lookup  (fromIntegral headerStreamId)  streamMap
      case curStream of
        Nothing -> if headerMsgType == YC.TypeData && headerLength > 0
                      then
                            lift $ do
                               logInfo sess $ "[WARN] yamux: Discarding data for stream:" <> show headerStreamId
                               _ <- NSB.recv conn (fromIntegral headerLength)
                               return ()
                      else lift $ logInfo sess $ "[WARN] yamux: frame for missing stream:" <> show hdr
        Just _  -> if headerMsgType == YC.TypeWindowUpdate 
                     then return ()
                     else return ()
 where
  synFlag = (headerFlags `Bit.xor` YC.flagSYN) == YC.flagSYN
  incomingStream::Int ->  TE.ExceptT String IO ()
  incomingStream streamID = do
    tryAssert "both yamux endpoints are clients" (isClient /=  even streamID)
    readLocalGoAway <- lift $ readTVarIO localGoAway
    if readLocalGoAway
      then do
            let sendHeader = YC.Header YC.protoVersion
                                       YC.TypeWindowUpdate
                                       YC.flagRST (fromIntegral streamID) 0
            sendMsg sendHeader Nothing
      else do
            stream <- lift $ newStream sess streamID StreamSYNReceived
            streamMap <- lift $ readTVarIO streams
            let mayStream = M.lookup streamID streamMap
            case mayStream of
              Nothing ->
                lift $ atomically $ do
                modifyTVar' streams (M.insert streamID stream)
                putTMVar acceptCh stream
              Just _  -> do
                lift $ logInfo sess "[ERR] yamux: duplicate stream declared"
                header <- lift $ goAway sess YC.goAwayProtoErr
                sendMsg header Nothing
                TE.throwE "duplicate stream initiated"

handlePing::Session -> YC.Header -> TE.ExceptT String IO ()
handlePing sess header = lift $ putStrLn "handlePing"


handleGoAway::Session -> YC.Header -> TE.ExceptT String IO ()
handleGoAway sess header = lift $ putStrLn "handleGoAway"

sendMsg::YC.Header -> Maybe BS.ByteString -> TE.ExceptT String IO ()
sendMsg header body = pure ()

goAway::Session -> Int -> IO YC.Header
goAway sess@Session{..} reason = do
  atomically $ swapTVar localGoAway True
  pure $ YC.Header YC.protoVersion YC.TypeGoAway 0 0 (fromIntegral reason)