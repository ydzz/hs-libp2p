{-#language OverloadedStrings,ScopedTypeVariables #-}

import Prelude
import qualified Libp2p.Multihash as M
import Data.Either
import qualified Crypto.Hash as Hash
import qualified Data.ByteString as BS
import Test.Hspec
import Data.HexString as Hex
main::IO ()
main = hspec $
  describe "test multihash" $ do
    it "test base58 = base58" $
      Right "QmPZ3ZfLNSiiQXj75Ft5Qs2qnPCt1Csp1nZBUppfY997Yb" `shouldBe` testBase58Hash "QmPZ3ZfLNSiiQXj75Ft5Qs2qnPCt1Csp1nZBUppfY997Yb"
    it "test hex = hex" $
      (Right $ Hex.hexString "122012087f891c5761d3a30cb08c324421e992d3247ff3a2603b946d292d50b76e20") `shouldBe`
      testHexHash (Hex.hexString "122012087f891c5761d3a30cb08c324421e992d3247ff3a2603b946d292d50b76e20")
    it "test hex = base58" $
      M.fromBS58String "QmPZ3ZfLNSiiQXj75Ft5Qs2qnPCt1Csp1nZBUppfY997Yb" `shouldBe`
      M.fromHexString  (Hex.hexString "122012087f891c5761d3a30cb08c324421e992d3247ff3a2603b946d292d50b76e20")
    it "read = base58" $
      ((Right $ read "QmPZ3ZfLNSiiQXj75Ft5Qs2qnPCt1Csp1nZBUppfY997Yb")::Either String M.Multihash) `shouldBe`
      M.fromBS58String "QmPZ3ZfLNSiiQXj75Ft5Qs2qnPCt1Csp1nZBUppfY997Yb"
    it "read = show" $ let hash::M.Multihash = read "QmPZ3ZfLNSiiQXj75Ft5Qs2qnPCt1Csp1nZBUppfY997Yb"
                       in  show hash `shouldBe` "QmPZ3ZfLNSiiQXj75Ft5Qs2qnPCt1Csp1nZBUppfY997Yb"


testBase58Hash::BS.ByteString -> Either String BS.ByteString
testBase58Hash str =  M.toBS58String <$> M.fromBS58String  str

testHexHash::Hex.HexString ->  Either String Hex.HexString
testHexHash str = M.toHexString <$> M.fromHexString str