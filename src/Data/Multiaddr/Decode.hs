module Data.Multiaddr.Decode
  (
    decode
  ) where

-- import qualified Data.ByteString as BSStrict

-- import Data.Bytes.VarInt (VarInt (..))
-- import Data.Serialize.Get (Get)

-- import Data.Multiaddr

-- -- parse was about get a multiaddrpart constructor, a string, and return a readp parser of multiaddr part
-- -- parseAddr was the same thing but with a function that worked on constructor that also took an address
-- -- what's with the Get monad?

-- parseBytes :: MultiaddrPart -> VarInt Int -> Get MultiaddrPart

-- parseAddrBytes :: (ByteString -> MultiaddrPart) -> VarInt Int -> Size -> Get MultiaddrPart

-- parseList :: ByteString -> [Int]
-- parseList = map fromIntegral . BSStrict.unpack

-- -- this time we take the ByteString and give back the Multiaddr
-- -- so we need to create the parser, but for the binary version
-- decode :: BSStrict.ByteString -> Either String Multiaddr
-- decode = runGetS $
--   Multiaddr <$> some
--     (
--       parseAddrBytes (IP4m . toIPV4 . toList) 4 (Size 32) <|>
--       parseAddrBytes (IP6m . toIPv6b . toList)
--     )

decode = undefined
