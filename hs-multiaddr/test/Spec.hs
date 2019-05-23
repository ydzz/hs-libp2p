{-#language OverloadedStrings,ScopedTypeVariables #-}
import Prelude
import Test.Hspec
import Libp2p.Multiaddr
import Data.Serialize
main::IO ()
main = do
  printTest
  hspec $ do
    describe "test protocol" $ do
     it "test read and show port" $ [TCP 666,UDP 666,DCCP 666,SCTP 2323] `shouldBe` map read ["/tcp/666","/udp/666","/dccp/666","/sctp/2323"]
     it "test read and show addr" $ [IP4 $ read "6.4.5.6",IP6 $ read "CDCD:910A:2222:5498:8475:1111:3900:2020"] `shouldBe`
                          map read ["/ip4/6.4.5.6","/ip6/CDCD:910A:2222:5498:8475:1111:3900:2020"]
     it "test read and show hash" $ [IPFS $ read "Qmd7aqZhb93HVZ5S4tyyF84dTbpN6SmgfdNPYgFB8wUyo8",P2P $ read "QmbLXJSkGZXpUoEc4ATTVhFmBFsm7o3otRAgCyR8D35PuX"] `shouldBe`
                          map read ["/ipfs/Qmd7aqZhb93HVZ5S4tyyF84dTbpN6SmgfdNPYgFB8wUyo8","/p2p/QmbLXJSkGZXpUoEc4ATTVhFmBFsm7o3otRAgCyR8D35PuX"]
    describe "test multiaddr" $
      it "test read multiaddr" $ [Multiaddr [IP4 $ read "1.193.37.20",TCP 2858]] `shouldBe` [read "/ip4/1.193.37.20/tcp/2858"]


printTest:: IO ()
printTest = do
  putStrLn "\r\n============"
  let p::Protocol = read "/tcp/2858"
  let bytes = encode p
  let pp::Either String Protocol = decode bytes
  print pp

  let addr::Multiaddr = read "/ip4/1.193.37.20/tcp/2858"
  print addr
  let abytes = encode addr
  let addr'::Either String Multiaddr  = decode abytes
  print addr'
  putStrLn "============"