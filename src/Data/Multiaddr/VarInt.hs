module Data.Multiaddr.VarInt
  (
    VarInt (..),
    encode,
    encodeWith,
    decode,
    decodeSize,
    decodeSizeVar
  ) where

import qualified Data.ByteString as BSStrict

import Data.Bytes.VarInt (VarInt (..))
import Data.Bytes.Get (getByteString)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (serialize, deserialize)

encode :: VarInt Int -> BSStrict.ByteString
encode = runPutS . serialize

encodeWith :: BSStrict.ByteString -> BSStrict.ByteString
encodeWith b = BSStrict.append (encode . fromIntegral . BSStrict.length b) b

decode :: Get (VarInt Int)
decode = deserialize

decodeSize :: Integral n => n -> Get BSStrict.ByteString
decodeSize n = getByteString $ fromIntegral $ div n 8

decodeSizeVar :: Get BSStrict.ByteString
decodeSizeVar = getByteString . fromIntegral =<< decode
