{-#LANGUAGE MultiParamTypeClasses#-}
{-#LANGUAGE AllowAmbiguousTypes#-}
{-#LANGUAGE FunctionalDependencies#-}
module Libp2p.Core.Mux (
  Closer(..),
  ReadWriteCloser(..),
  MuxedStream(..),
  MuxedConn(..),
  Multiplexer(..)
) where
import　qualified System.IO.Streams as S
import Data.Word
import Network.Socket

class Closer a where
 close:: a -> m ()

class (Closer a) => ReadWriteCloser a where
    read::a  -> S.InputStream Word8
    write::a -> S.OutputStream Word8

class (Closer a) => MuxedStream a where
  intputStream::a -> S.InputStream Word8
  outputStream::a -> S.OutputStream Word8
  reset::a -> Maybe String
  setDeadline:: a -> Int -> m (Maybe String)
  setReadDeadline:: a -> Int -> m (Maybe String)
  setWriteDeadline:: a -> Int -> m (Maybe String)


class (Closer a,MuxedStream b) => MuxedConn a b | a -> b where
  isClosed::a   -> m Bool
  openStream::a -> m (Either String b)
  acceptStream::a -> m (Either String b)



newtype Multiplexer m a = Multiplexer { newConn::Socket -> Bool -> m a }
