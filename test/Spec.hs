{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables#-}
import Libp2p.Core.Crypto.RSA
import qualified Libp2p.Core.Crypto.Key as K
import Numeric (showHex)
import qualified Data.ByteString as B
import qualified System.IO as I
import Data.Either
import Libp2p.Core.Peer.Internal
import Text.Read
import qualified Libp2p.Multihash as MH
main :: IO ()
main = do
 putStrLn "\r\n========================"
 testPeer

testCrypto::IO ()
testCrypto = do
 (pub,prv) <- generateRSAKeyPair 64
 print $ K.bytes pub
 print $ K.raw pub
 print $ K.typ pub
 print $ K.bytes prv
 putStrLn "==================="
 let signData::B.ByteString = "12345691011121314151617181920"
 let   signRet = K.sign prv signData
 print signRet
 let signToken::B.ByteString = fromRight "" signRet 
 print $ K.verify pub signData signToken
 --B.writeFile "fff.txt" $  raw pub
 --B.writeFile "ppp.txt" privareBS


testPeer::IO ()
testPeer = do
  let pid = ID "000000000000000000000000000000000000000000"
  print $ pretty　pid
  print pid
  let pid2::Either String ID = fromBS58String "QmX9c6GNBsoFBwQLSBNKhygi32iLzg4oWHXwxGDRBDo6qn"
  print pid2
  let hexString =  toHexString (fromRight undefined pid2)
  print $ fromHexString hexString

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