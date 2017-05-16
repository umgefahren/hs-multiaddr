module Data.Multiaddr.Encode
  (
    encode
  ) where

import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Char8 as BSStrictChar
import qualified Data.Multihash.Digest as MHD

import Data.IP (fromIPv4, fromIPv6b)
import Data.Bytes.VarInt (VarInt (..))
import Data.Bytes.Get (getByteString, runGetS)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (serialize)

import Data.Multiaddr (Multiaddr (..), MultiaddrPart (..))

encodeList :: Integral a => [a] -> BSStrict.ByteString
encodeList = BSStrict.pack . map fromIntegral

-- fixed-width types passed to serialize gets serialised as big endian
-- multiaddr ports must be encoded big-endian
encodePort :: Port -> BSStrict.ByteString
encodePort p = (runPutS . serialize) $ port p

encodeVarInt :: VarInt Int -> BSStrict.ByteString
encodeVarInt = runPutS . serialize

encodeAddr :: BSStrict.ByteString -> BSStrict.ByteString
encodeAddr b = BSStrict.append (encodeVarInt . fromIntegral . BSStrict.length $ b) b

toBytes :: MultiaddrPart -> BSStrict.ByteString
toBytes (IP4m i)   = BSStrict.append (encodeVarInt 4) (encodeList. fromIPv4 $ i)
toBytes (IP6m i)   = BSStrict.append (encodeVarInt 41) (encodeList . fromIPv6b $ i)
toBytes (TCPm p)   = BSStrict.append (encodeVarInt 6) (encodePort p)
toBytes (UDPm p)   = BSStrict.append (encodeVarInt 17) (encodePort p)
toBytes (DCCPm p)  = BSStrict.append (encodeVarInt 33) (encodePort p)
toBytes (SCTPm p)  = BSStrict.append (encodeVarInt 132) (encodePort p)
toBytes (ONIONm o) = BSStrict.concat [(encodeVarInt 444), (encodeAddr $ onionHash o), (encodePort $ onionPort o)]
toBytes (IPFSm h)  = BSStrict.append (encodeVarInt 421) (encodeAddr $ MHD.digest h)
toBytes (P2Pm h)   = BSStrict.append (encodeVarInt 420) (encodeAddr $ MHD.digest h)
toBytes (UNIXm p)  = BSStrict.append (encodeVarInt 400) (encodeAddr $ BSStrictChar.pack $ path p)
toBytes UTPm       = encodeVarInt 302
toBytes UDTm       = encodeVarInt 301
toBytes QUICm      = encodeVarInt 81
toBytes HTTPm      = encodeVarInt 480
toBytes HTTPSm     = encodeVarInt 443
toBytes WSm        = encodeVarInt 477
toBytes WSSm       = encodeVarInt 478

encode :: Multiaddr -> BSStrict.ByteString
encode (Multiaddr parts) = BSStrict.concat . map toBytes $ parts
