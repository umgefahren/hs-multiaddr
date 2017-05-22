module Data.Multiaddr.IPFS
  (
    MHD.MultihashDigest (..),
    toString,
    parse,
    encode,
    parseB
  ) where

import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Lazy.Char8 as BSLazyChar
import qualified Data.Multiaddr.VarInt as VarInt
import qualified Data.Multihash.Digest as MHD
import qualified Data.Multihash.Base as MHB

toString :: MHD.MultihashDigest -> String
toString =
  BSLazyChar.unpack $
  MHB.encode MHB.Base58
  (BSLazy.fromStrict $ MHD.digest h)

parse :: ReadP MHD.MultihashDigest
parse = do
  multihashText <- Parser.munch1 (/= '/')
  case ((MHB.decode MHB.Base58) (BSLazyChar.pack multihashText)) of
    Right bs ->
      case MHD.decode (BSLazyChar.toStrict bs) of
        Right digest -> return digest
        otherwise -> Parser.pfail
    otherwise -> Parser.pfail

encode :: MHD.MultihashDigest -> BSStrict.ByteString
encode h = VarInt.encodeWith $ MHD.digest h

parseB :: Get MHD.MultihashDigest
parseB = VarInt.decodeSizeVar >>= (either fail return . MHD.decode)
