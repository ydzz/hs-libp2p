{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Libp2p.Core.Crypto.PB.PublicKey (PublicKey(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Libp2p.Core.Crypto.PB.KeyType as Libp2p.Core.Crypto.PB (KeyType)

data PublicKey = PublicKey{type' :: !(Libp2p.Core.Crypto.PB.KeyType), data' :: !(P'.ByteString)}
                 deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data, Prelude'.Generic)

instance P'.Mergeable PublicKey where
  mergeAppend (PublicKey x'1 x'2) (PublicKey y'1 y'2) = PublicKey (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)

instance P'.Default PublicKey where
  defaultValue = PublicKey P'.defaultValue P'.defaultValue

instance P'.Wire PublicKey where
  wireSize ft' self'@(PublicKey x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 14 x'1 + P'.wireSizeReq 1 12 x'2)
  wirePutWithSize ft' self'@(PublicKey x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> put'FieldsSized
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields = P'.sequencePutWithSize [P'.wirePutReqWithSize 8 14 x'1, P'.wirePutReqWithSize 18 12 x'2]
        put'FieldsSized
         = let size' = Prelude'.fst (P'.runPutM put'Fields)
               put'Size
                = do
                    P'.putSize size'
                    Prelude'.return (P'.size'WireSize size')
            in P'.sequencePutWithSize [put'Size, put'Fields]
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = new'Field}) (P'.wireGet 14)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{data' = new'Field}) (P'.wireGet 12)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self

instance P'.MessageAPI msg' (msg' -> PublicKey) PublicKey where
  getVal m' f' = f' m'

instance P'.GPB PublicKey

instance P'.ReflectDescriptor PublicKey where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8, 18]) (P'.fromDistinctAscList [8, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Libp2p.Core.Crypto.PB.PublicKey\", haskellPrefix = [], parentModule = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\"], baseName = MName \"PublicKey\"}, descFilePath = [\"Libp2p\",\"Core\",\"Crypto\",\"PB\",\"PublicKey.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Libp2p.Core.Crypto.PB.PublicKey.Type\", haskellPrefix' = [], parentModule' = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\",MName \"PublicKey\"], baseName' = FName \"type'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Libp2p.Core.Crypto.PB.KeyType\", haskellPrefix = [], parentModule = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\"], baseName = MName \"KeyType\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Libp2p.Core.Crypto.PB.PublicKey.Data\", haskellPrefix' = [], parentModule' = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\",MName \"PublicKey\"], baseName' = FName \"data'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}"

instance P'.TextType PublicKey where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage

instance P'.TextMsg PublicKey where
  textPut msg
   = do
       P'.tellT "Type" (type' msg)
       P'.tellT "Data" (data' msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'type', parse'data']) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'type'
         = P'.try
            (do
               v <- P'.getT "Type"
               Prelude'.return (\ o -> o{type' = v}))
        parse'data'
         = P'.try
            (do
               v <- P'.getT "Data"
               Prelude'.return (\ o -> o{data' = v}))