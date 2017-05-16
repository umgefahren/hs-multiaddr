{-# LANGUAGE DeriveGeneric #-}

module Data.Multiaddr.Port
  (
    Port (..)
  ) where

import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict

import GHC.Generics (Generic)
import Data.Word (Word16)
import Data.Char (isDigit)

newtype Port = Port
  {
    port :: Word16
  }
  deriving (Show, Eq, Generic)

instance Read Port where
  readsPrec _ = Parser.readP_to_S $ do
    port <- fmap read $ rangeParse (Parser.satisfy isDigit) 1 5 :: Parser.ReadP Int
    case toPort port of
      Just p -> return p
      Nothing -> Parser.pfail

rangeParse :: Read a => Parser.ReadP a -> Int -> Int -> Parser.ReadP [a]
rangeParse parse min max = foldr1 (Parser.<++) $
  fmap (\n -> Parser.count n $ parse) [max, max-1..min]

toPort :: (Integral a, Eq a) => a -> Maybe Port
toPort n | n < 0 || n > 65535 = Nothing
         | otherwise          = Just $ Port (fromIntegral n)

fromPort :: Integral a => Port -> a
fromPort p = fromIntegral (port p)
