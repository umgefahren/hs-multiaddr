{-# LANGUAGE DeriveGeneric #-}

module Data.Multiaddr.Onion
  (
    Onion (..),
    toString,
    parse,
    encode,
    decode
  ) where

import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Char8 as BSStrictChar
import qualified Codec.Binary.Base32 as Base32
import qualified Data.Multiaddr.VarInt as VarInt
import qualified Data.Multiaddr.Port as Port

import GHC.Generics (Generic)
import Data.Char (isAlphaNum, toUpper)

data Onion = Onion
  {
    onionHash :: {-# UNPACK #-} !BSStrict.ByteString,
    onionPort :: {-# UNPACK #-} !Port.Port
  }
  deriving (Show, Eq, Generic)

toString :: Onion -> String
toString (Onion h p) =
  hashToStr ++ ":" ++ portToStr
  where
    hashToStr = map toLower . BSStrictChar.unpack $ Base32.encode h
    portToStr = Port.toString p

parse :: Parser.ReadP Onion
parse = do
  onionHash <- fmap
    (BSStrictChar.pack . map toUpper)
    (Parser.count 16 $ Parser.satisfy isAlphaNum)
  case Base32.decode onionHash of
    Right onionHashDecoded -> do
      Parser.char ':'
      onionPort <- Port.parse
      if port onionPort > 0
        then return $ Onion onionHashDecoded onionPort
        else Parser.pfail
    otherwise -> Parser.pfail

encode :: Onion -> BSStrict.ByteString
encode (Onion h p) = BSStrict.concat [(VarInt.encode 10), h, (Port.encode p)]

decode :: Get Onion
decode = do
  i <- VarInt.decode
  unless (i == 10) $ fail "Wrong protocol index"
  b <- VarInt.decodeSize 96
  let (h, p) = BSStrict.splitAt 10 b
  p <- Port.decode p
  return $ Onion h p
