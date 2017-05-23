module Data.Multiaddr.IPv6
  (
    IPv6 (..),
    toString,
    parse,
    encode,
    parseB
  ) where

import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict
import qualified Data.Multiaddr.VarInt as VarInt

import Data.IP (IPv6 (..), fromIPv6b, toIPv6b)
import Data.Serialize.Get (Get)

toString :: IPv6 -> String
toString = show

parse :: Parser.ReadP IPv6
parse = Parser.readS_to_P reads

encode :: IPv6 -> BSStrict.ByteString
encode = BSStrict.pack . map fromIntegral . fromIPv6b

parseB :: Get IPv6
parseB =
  fmap (toIPv6b . map fromIntegral . BSStrict.unpack) $
  VarInt.decodeSize 128
