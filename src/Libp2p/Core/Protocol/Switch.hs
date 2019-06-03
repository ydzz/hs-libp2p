module Libp2p.Core.Protocol.Switch (
    ID,
    HandlerFunc(..),
    Router(..),
    Negotiator(..),
    Switch(..)
) where
import qualified Libp2p.Core.Mux as MUX
import Data.ByteString

type ID = ByteString

newtype HandlerFunc m a = HandlerFunc {  handerl::String -> a -> m () } 
class Router a where
    protocols::a ->　m [String]
    addHandler:: (MUX.ReadWriteCloser b) => a -> HandlerFunc m b -> m a
    addHandlerWithFunc:: (MUX.ReadWriteCloser b) => a -> (String -> m Bool) -> HandlerFunc m b -> m a
    removeHandler::a -> String -> m a

class Negotiator a where
    ngotiateLazy:: (MUX.ReadWriteCloser b) => a -> b -> m (Either String (b,String,HandlerFunc m b))
    negotiate:: (MUX.ReadWriteCloser b) => a -> b -> m (Either String (String,HandlerFunc m b))
    handle::(MUX.ReadWriteCloser b) => a -> b -> Maybe String

class (Router a,Negotiator a) => Switch a where