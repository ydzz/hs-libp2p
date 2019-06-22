{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables#-}
{-# LANGUAGE TypeSynonymInstances#-}
{-# LANGUAGE FlexibleInstances#-}
{-# LANGUAGE MultiParamTypeClasses#-}
import Libp2p.Core.Crypto.RSA
import qualified Libp2p.Core.Crypto as K
import Numeric (showHex)
import qualified Data.ByteString as B
import qualified System.IO as I
import Data.Either
import Libp2p.Core.Peer.Peer
import Text.Read
import Data.Default
import System.Log.FastLogger
import qualified Libp2p.Multihash as MH
import Yamux.Session as YS
import Yamux.Config as YC
import Control.Monad
import System.Time.Extra
import Network.Socket
import qualified Control.Exception as E
import Control.Concurrent.Async
import Control.Concurrent
import Yamux.Deadline
import Data.Time

main :: IO ()
main = do
 putStrLn "\r\n========================"
 testDeadline

testDeadline::IO ()
testDeadline = do
  putStrLn "start"
  line <- mkPipeDeadline
  now <- getCurrentTime
  let newTime  = addUTCTime (fromRational 5) now
  let newTime2 = addUTCTime (fromRational 3) now
  print newTime
  print newTime2
  setDeadline line newTime
  setDeadline line newTime2
  waitDeadline line
  now <- getCurrentTime
  print now
  putStrLn "end"


testCrypto::IO ()
testCrypto = do
 (pub,prv) <- generateRSAKeyPair 64
 putStrLn "==================="
 let signData::B.ByteString = "12345691011121314151617181920"
 let   signRet = K.sign (K.RSAPrivateKey prv) signData
 print signRet
 let signToken::B.ByteString = fromRight "" signRet
 print $ K.verify (K.RSAPubKey pub) signData signToken
 --B.writeFile "fff.txt" $  raw pub
 --B.writeFile "ppp.txt" privareBS


testPeer::IO ()
testPeer = do
  let pid = ID "000000000000000000000000000000000000000000"
  print $ pretty pid
  print pid
  let pid2::Either String ID = fromBS58String "QmX9c6GNBsoFBwQLSBNKhygi32iLzg4oWHXwxGDRBDo6qn"
  print pid2
  let hexString =  toHexString (fromRight undefined pid2)
  print $ fromHexString hexString


testLogger::IO ()
testLogger = do
 logdst <- newFileLoggerSet 1024 "/home/yangdao/Project/a.txt"
 pushLogStrLn logdst "123456"
 pushLogStrLn logdst "1234566"
 pushLogStrLn logdst "12345654"
 pushLogStrLn logdst "1234563"
 return ()

testSession::IO ()
testSession = do
   addr <- resolve "3001"
   sock <- open addr
   loop sock
   sleep 3
   putStrLn "End Test Session"
 where
  resolve port = do
    let hints = defaultHints {
            addrFlags = [AI_PASSIVE]
          , addrSocketType = Stream
          }
    addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
    return addr
  open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption sock ReuseAddr 1
    fd <- fdSocket sock
    setCloseOnExecIfNeeded fd
    bind sock (addrAddress addr)
    listen sock 5
    putStrLn $ "open " <> show (addrAddress addr)
    return sock
  loop sock = forever $ do
        putStrLn "start loop"
        (conn, peer) <- accept sock
        putStrLn $ "Connection from " ++ show peer
        sess <- YS.newSession def conn False 0
        return ()


{-

pN::Integer
pN = 9539842081174255871466990106524269442826697681552037238467331932977137065371865834043219892102089357661106245490218817523177153503724063796934425007050521

d = 3896754085677324712439863361942943573622086713385538503040579761749820090025475909392656880153804138231283413195717524362631695026137970691842913509790473
p = 101732542866484830597614910440649518318755687390601020500488368560652039408931
q = 93773750388747030929105198793311675757387418386724432121439259852703759903891
dp = 98073791267597106934362421863073326020095279450358919010953432803790162043673
dQ = 89572768788408881749132773084823904269164132908545343473674704457621009105443
qinv = 48019368095230028020649151593702171443677147195415887626298240560880232186249

testPrivateKey::RSAPrivateKey
testPrivateKey = R.PrivateKey (R.PublicKey 64 pN 0x10001) d p q dp dQ qinv

privareBS = rawPrivate testPrivateKey

-}