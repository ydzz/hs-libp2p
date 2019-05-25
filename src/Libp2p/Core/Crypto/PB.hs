{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC  -fno-warn-unused-imports #-}
module Libp2p.Core.Crypto.PB (protoInfo, fileDescriptorProto) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified GHC.Generics as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import Text.DescriptorProtos.FileDescriptorProto (FileDescriptorProto)
import Text.ProtocolBuffers.Reflections (ProtoInfo)
import qualified Text.ProtocolBuffers.WireMessage as P' (wireGet,getFromBS)

protoInfo :: ProtoInfo
protoInfo
 = Prelude'.read
    "ProtoInfo {protoMod = ProtoName {protobufName = FIName \".Libp2p.Core.Crypto.PB\", haskellPrefix = [], parentModule = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\"], baseName = MName \"PB\"}, protoFilePath = [\"Libp2p\",\"Core\",\"Crypto\",\"PB.hs\"], protoSource = \"crypto.proto\", extensionKeys = fromList [], messages = [DescriptorInfo {descName = ProtoName {protobufName = FIName \".Libp2p.Core.Crypto.PB.PublicKey\", haskellPrefix = [], parentModule = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\"], baseName = MName \"PublicKey\"}, descFilePath = [\"Libp2p\",\"Core\",\"Crypto\",\"PB\",\"PublicKey.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Libp2p.Core.Crypto.PB.PublicKey.Type\", haskellPrefix' = [], parentModule' = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\",MName \"PublicKey\"], baseName' = FName \"type'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Libp2p.Core.Crypto.PB.KeyType\", haskellPrefix = [], parentModule = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\"], baseName = MName \"KeyType\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Libp2p.Core.Crypto.PB.PublicKey.Data\", haskellPrefix' = [], parentModule' = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\",MName \"PublicKey\"], baseName' = FName \"data'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False},DescriptorInfo {descName = ProtoName {protobufName = FIName \".Libp2p.Core.Crypto.PB.PrivateKey\", haskellPrefix = [], parentModule = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\"], baseName = MName \"PrivateKey\"}, descFilePath = [\"Libp2p\",\"Core\",\"Crypto\",\"PB\",\"PrivateKey.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Libp2p.Core.Crypto.PB.PrivateKey.Type\", haskellPrefix' = [], parentModule' = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\",MName \"PrivateKey\"], baseName' = FName \"type'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Libp2p.Core.Crypto.PB.KeyType\", haskellPrefix = [], parentModule = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\"], baseName = MName \"KeyType\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Libp2p.Core.Crypto.PB.PrivateKey.Data\", haskellPrefix' = [], parentModule' = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\",MName \"PrivateKey\"], baseName' = FName \"data'\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False, makeLenses = False}], enums = [EnumInfo {enumName = ProtoName {protobufName = FIName \".Libp2p.Core.Crypto.PB.KeyType\", haskellPrefix = [], parentModule = [MName \"Libp2p\",MName \"Core\",MName \"Crypto\",MName \"PB\"], baseName = MName \"KeyType\"}, enumFilePath = [\"Libp2p\",\"Core\",\"Crypto\",\"PB\",\"KeyType.hs\"], enumValues = [(EnumCode {getEnumCode = 0},\"RSA\"),(EnumCode {getEnumCode = 1},\"Ed25519\"),(EnumCode {getEnumCode = 2},\"Secp256k1\"),(EnumCode {getEnumCode = 3},\"ECDSA\")]}], oneofs = [], knownKeyMap = fromList []}"

fileDescriptorProto :: FileDescriptorProto
fileDescriptorProto
 = P'.getFromBS (P'.wireGet 11)
    (P'.pack
      "\251\SOH\n\fcrypto.proto\DC2\NAKLibp2p.Core.Crypto.PB\"G\n\tPublicKey\DC2,\n\EOTType\CAN\SOH \STX(\SO2\RS.Libp2p.Core.Crypto.PB.KeyType\DC2\f\n\EOTData\CAN\STX \STX(\f\"H\n\nPrivateKey\DC2,\n\EOTType\CAN\SOH \STX(\SO2\RS.Libp2p.Core.Crypto.PB.KeyType\DC2\f\n\EOTData\CAN\STX \STX(\f*9\n\aKeyType\DC2\a\n\ETXRSA\DLE\NUL\DC2\v\n\aEd25519\DLE\SOH\DC2\r\n\tSecp256k1\DLE\STX\DC2\t\n\ENQECDSA\DLE\ETXb\ACKproto2")