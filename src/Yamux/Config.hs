{-#LANGUAGE RecordWildCards #-}
module Yamux.Config where
import Data.Default
import Yamux.Const
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.Except
import Control.Error.Safe
import Data.Either
import Control.Monad.Extra
import qualified System.IO.Streams as S

data Config = Config {
  acceptBacklog::Int,
  enableKeepAlive::Bool,
  keepAliveInterval::Int,
  connectionWriteTimeout::Int,
  maxStreamWindowSize::Int,
  logOutput::Maybe (S.OutputStream String),
  readBufSize::Int,
  writeCoalesceDelay::Int,
  maxMessageSize::Int
}

instance Default Config where
    def = Config {
        acceptBacklog = 256,
        enableKeepAlive = True,
        keepAliveInterval = 30 * 1000,
        connectionWriteTimeout = 10 * 1000,
        maxStreamWindowSize    = initialStreamWindow,
        readBufSize            = 4096,
        maxMessageSize         = 64 * 1024,
        writeCoalesceDelay     = 100,
        logOutput = Nothing
    }

verifyConfig::Config -> Either String Config
verifyConfig cfg@Config{..} = runExcept $ do
   tryAssert "backlog must be positive" (acceptBacklog < 0)
   tryAssert "KeepAliveInterval" (keepAliveInterval == 0)
   tryAssert  (mconcat ["MaxStreamWindowSize must be larger than ", show initialStreamWindow ,"initialStreamWindow"])
          (maxStreamWindowSize < initialStreamWindow)
   tryAssert "MaxMessageSize must be greater than a kilobyte" (maxMessageSize < 1024)
   tryAssert "WriteCoalesceDelay must be >= 0" (writeCoalesceDelay >= 0)
   pure cfg