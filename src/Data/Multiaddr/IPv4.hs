module Data.Multiaddr.IPv4
  (
    IPv4 (..),
    toString,
    parse,
    encode,
    parseB
  ) where

import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict
import qualified Data.Multiaddr.VarInt as VarInt

import Data.IP (IPv4 (..), fromIPv4, toIPv4)

toString :: IPv4 -> String
toString = show

parse :: ReadP IPv4
parse = Parser.readS_to_P reads

encode :: IPv4 -> BSStrict.ByteString
encode = BSStrict.pack . map fromIntegral . fromIPv4

parseB :: Get IPv4
parseB =
  fmap (toIPv4 . map fromIntegral . BSStrict.unpack) $
  VarInt.decodeSize 32
