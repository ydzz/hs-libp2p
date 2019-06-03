module Yamux.Mux where
import Data.Int
import GHC.Generics
import Yamux.Config
import Data.Default
import Network.Socket
import Data.Maybe

mkClient::Socket -> Maybe Config -> IO (Either String String)
mkClient _ mayConfig = do
  let vCfg = verifyConfig (fromMaybe (def::Config) mayConfig)
  either (pure . Left) (\cfg -> pure $ Right "") vCfg