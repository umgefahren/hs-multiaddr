{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Data.Multiaddr
  (
    -- * As Multiaddr types
    Multiaddr (..),
    P.MultiaddrPart (..),
    -- * As a string
    toString,
    toMultiaddr,
    -- * As binary packed format
    encode,
    decode,
    -- * Utilities
    P.partHdrS,
    P.partHdrB,
    encapsulate,
    decapsulate,
    findFirstPart,
    findLastPart,
    findAllParts
  ) where

import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict
import qualified Data.MultiaddrPart as P

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.String (IsString, fromString)
import Control.Applicative (many, some)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.Serialize.Get (Get)
import Data.List (isPrefixOf, find, filter)

newtype Multiaddr = Multiaddr { parts :: [P.MultiaddrPart] }
  deriving (Show, Eq, Monoid, Generic, Typeable)

instance IsString Multiaddr where
  fromString s = either error id $ toMultiaddr s

toString :: Multiaddr -> String
toString (Multiaddr p) = concatMap P.toString p

toMultiaddr :: String -> Either String Multiaddr
toMultiaddr s =
  case Parser.readP_to_S (parse <* Parser.eof) s of
    (m, ""):_ -> Right m
    otherwise -> Left $ "Data.Multiaddr.toMultiaddr: invalid multiaddr " ++ show s

parse :: Parser.ReadP Multiaddr
parse = do
  multiParts <- some P.parse
  many $ Parser.char '/'
  return $ Multiaddr multiParts

encode :: Multiaddr -> BSStrict.ByteString
encode (Multiaddr p) = BSStrict.concat . map P.encode $ p

decode :: BSStrict.ByteString -> Either String Multiaddr
decode = runGetS $ parseB

parseB :: Get Multiaddr
parseB = Multiaddr <$> some P.parseB

-- first address encapsulates the second address
encapsulate :: Multiaddr -> Multiaddr -> Multiaddr
encapsulate = mappend

-- first address is prefix removed from the second address
decapsulate :: Multiaddr -> Multiaddr -> Maybe Multiaddr
decapsulate (Multiaddr p1) (Multiaddr p2)
  | isPrefixOf p1 p2 = Just . Multiaddr . drop (length p1) $ p2
  | otherwise = Nothing

findFirstPart :: P.MultiaddrPart -> Multiaddr -> Maybe P.MultiaddrPart
findFirstPart p a = find (\p' -> P.partHdrB p' == pI) (parts a)
  where
    pI = P.partHdrB p

findLastPart :: P.MultiaddrPart -> Multiaddr -> Maybe P.MultiaddrPart
findLastPart p a = go (\p' -> P.partHdrB p' == pI) (parts a) Nothing
  where
    pI = P.partHdrB p
    go _ [] lastPart = lastPart
    go pred (p:ps) lastPart
      | pred p    = go pred ps $ Just p
      | otherwise = go pred ps lastPart

findAllParts :: P.MultiaddrPart -> Multiaddr -> [P.MultiaddrPart]
findAllParts p a = filter (\p' -> P.partHdrB p' == pI) (parts a)
  where
    pI = P.partHdrB p
