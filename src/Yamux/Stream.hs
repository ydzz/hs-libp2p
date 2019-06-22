{-#Language RecordWildCards#-}
module Yamux.Stream where
import Yamux.Types
import qualified Yamux.Const as YC
import qualified Data.ByteString as BS
import Control.Concurrent.STM.TBQueue
import qualified Control.Monad.Trans.Except as TE

newStream::Session -> Int -> StreamState -> IO Stream
newStream session streamId state = do
  let recvWindow = YC.initialStreamWindow
  let sendWindow = YC.initialStreamWindow
  let recvBuf = BS.empty
  recvNotifyCh <- newTBQueueIO 1
  sendNotifyCh <- newTBQueueIO 1
  pure $ Stream{..}

incrSendWindow::YC.Header -> Int -> TE.ExceptT String IO ()
incrSendWindow hdr flags = return ()

processFlags::Stream -> Int -> TE.ExceptT String IO ()
processFlags stream flags = do
  return ()