{-#Language RecordWildCards#-}
module Yamux.Stream where
import Yamux.Types
import qualified Yamux.Const as YC
import qualified Data.ByteString as BS
import qualified Control.Monad.Trans.Except as TE
newStream::Session -> Int -> StreamState -> IO Stream
newStream session streamId state = do
  let recvWindow = YC.initialStreamWindow
  let sendWindow = YC.initialStreamWindow
  let recvBuf = BS.empty
  pure $ Stream{..}

incrSendWindow::YC.Header -> Int -> TE.ExceptT String IO ()
incrSendWindow hdr flags = return ()

processFlags::Stream -> Int -> TE.ExceptT String IO ()
processFlags stream flags = do
  return ()