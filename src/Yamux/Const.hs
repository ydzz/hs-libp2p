{-#LANGUAGE RecordWildCards#-}
{-#LANGUAGE ScopedTypeVariables#-}
module Yamux.Const (
    initialStreamWindow,
    Header(..),
    headerSize,
    protoVersion,
    MsgType(..)
) where
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Word
import qualified Prelude.SafeEnum as SE
import Data.Bits

initialStreamWindow::Int
initialStreamWindow = 256 * 1024

protoVersion = 0::Word8

flagSYN = 1::Word16
flagACK = 1 `shift` 1::Word16
flagFIN = 1 `shift` 2::Word16
flagRST = 1 `shift` 3::Word16

sizeOfVersion = 1::Int
sizeOfType = 1::Int
sizeOfFlags = 2::Int
sizeOfStreamID = 4::Int
sizeOfLength = 4::Int

headerSize::Int
headerSize = sizeOfVersion + sizeOfType + sizeOfFlags + sizeOfStreamID + sizeOfLength

data MsgType = TypeData | TypeWindowUpdate | TypePing | TypeGoAway deriving (Eq,Show)


intToMsgType::Int -> Maybe MsgType
intToMsgType 0 = Just TypeData
intToMsgType 1 = Just TypeWindowUpdate
intToMsgType 2 = Just TypePing
intToMsgType 3 = Just TypeGoAway
intToMsgType _ = Nothing

msgTypeToInt::MsgType -> Int
msgTypeToInt TypeData = 0
msgTypeToInt TypeWindowUpdate = 1
msgTypeToInt TypePing = 2
msgTypeToInt TypeGoAway = 3

data Header = Header {
    headerVersion::Word8,
    headerMsgType::MsgType,
    headerFlags::Word16,
    headerStreamId::Word32,
    headerLength::Word32
} deriving (Show)

instance Serialize Header where
  put Header{..} = do
    putWord8 headerVersion
    putWord8 (fromIntegral $ msgTypeToInt headerMsgType)
    putWord16be headerFlags
    putWord32be headerStreamId
    putWord32be headerLength
  get = do
    headerVersion     <- getWord8
    headerMsgTypeNum  <- getWord8
    let mayMsgType::Maybe MsgType = intToMsgType $ fromIntegral headerMsgTypeNum
    case mayMsgType of
      Nothing -> fail "msgtype error"
      Just  headerMsgType  -> do
                   headerFlags    <- getWord16be
                   headerStreamId <- getWord32be
                   headerLength   <- getWord32be
                   return Header{..}