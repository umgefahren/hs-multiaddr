{-# LANGUAGE DeriveGeneric #-}

module Data.Multiaddr.UnixPath
  (
    UnixPath (..),
    toString,
    parse,
    encode,
    decode
  ) where

import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Char8 as BSStrictChar
import qualified Data.Multiaddr.VarInt as VarInt

import GHC.Generics (Generic)
import System.FilePath (FilePath)
-- import Data.Maybe (listToMaybe)

newtype UnixPath = UnixPath { path :: FilePath }
                 deriving (Eq, Ord, Generic)

toString :: UnixPath -> String
toString (UnixPath p) = show p

parse :: ReadP UnixPath
parse = do
  path <- Parser.many1 Parser.get
  Parser.manyTill (Parser.char '/') Parser.eof
  return $ UnixPath $ "/" ++ path

encode :: UnixPath -> BSStrict.ByteString
encode (UnixPath p) = VarInt.encodeWith $ BSStrictChar.pack p

decode :: Get UnixPath
decode = fmap (UnixPath . BSStrictChar.unpack) $ VarInt.decodeSizeVar

-- instance Read UnixPath where
--   readsPrec _ = fmap (maybe [] (:[]) . listToMaybe) $ Parser.readP_to_S $ do
--     path <- Parser.many1 Parser.get
--     Parser.manyTill (Parser.char '/') Parser.eof
--     return $ UnixPath $ "/" ++ path
