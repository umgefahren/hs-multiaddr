{-# LANGUAGE DeriveGeneric #-}

module Data.Multiaddr.Port
  (
    Port (..),
    toString,
    parse,
    encode,
    parseB,
    toPort,
    fromPort
  ) where

import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict

import GHC.Generics (Generic)
import Data.Word (Word16)
import Data.Char (isDigit)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (serialize, deserialize)

newtype Port = Port
  {
    port :: Word16
  }
  deriving (Eq, Ord, Generic)

toPort :: (Integral a, Eq a) => a -> Maybe Port
toPort n | n < 0 || n > 65535 = Nothing
         | otherwise          = Just $ Port (fromIntegral n)

fromPort :: Integral a => Port -> a
fromPort p = fromIntegral (port p)

toString :: Port -> String
toString (Port p) = show p

parse :: Parser.ReadP Port
parse = do
  port <- fmap read $ rangeParse (Parser.satisfy isDigit) 1 5 :: Parser.ReadP Int
  case toPort port of
    Just p -> return p
    Nothing -> Parser.pfail

rangeParse :: Read a => Parser.ReadP a -> Int -> Int -> Parser.ReadP [a]
rangeParse parse min max = foldr1 (Parser.<++) $
  fmap (\n -> Parser.count n $ parse) [max, max-1..min]

encode :: Port -> BSStrict.ByteString
encode (Port p) = (runPutS . serialize) $ p

parseB :: BSStrict.ByteString -> Get Port
parseB = fmap (Port . deserialize) $ VarInt.decodeSize 16
