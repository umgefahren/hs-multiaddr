{-# LANGUAGE DeriveGeneric #-}

module Data.Multiaddr.Onion
  (
    Onion (..)
  ) where

import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Char8 as BSStrictChar
import qualified Codec.Binary.Base32 as Base32

import GHC.Generics (Generic)
import Data.Char (isAlphaNum)
import Data.Multiaddr.Port

data Onion = Onion
  {
    onionHash :: BSStrict.ByteString,
    onionPort :: Port
  }
  deriving (Show, Eq, Generic)

instance Read Onion where
  readsPrec _ = Parser.readP_to_S $ do
    onionHash <- BSStrictChar.pack <$>
      (Parser.count 16 $ Parser.satisfy isAlphaNum)
    case Base32.decode onionHash of
      Right onionHashDecoded -> do
        Parser.char ':'
        onionPort <- Parser.readS_to_P reads :: Parser.ReadP Port
        if port onionPort > 0
          then return $ Onion onionHashDecoded onionPort
          else Parser.pfail
      otherwise -> Parser.pfail
