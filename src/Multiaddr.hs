{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Multiaddr
  (
    -- * As Multiaddr types
    Multiaddr (..),
    MultiaddrPart (..),
    Port (..),
    Onion (..),
    UnixPath (..),
    -- * As a string
    show,
    read,
    -- * As binary packed format
    encode,
    decode,
    -- * Utilities
    fromPort,
    toPort
    -- encapsulate,
    -- decapsulate,
    -- startsWith
  ) where

import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.Multihash.Digest as MHD
import qualified Data.Multihash.Base as MHB
import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Char8 as BSStrictChar
import qualified Data.ByteString.Lazy.Char8 as BSLazyChar
import qualified Data.ByteString.Base32 as BSBase32

import GHC.Generics (Generic)
import System.FilePath (FilePath)
import Data.Char (isAlphaNum, isDigit)
import Data.Word (Word16)
import Data.IP (IPv4, IPv6, fromIPv4, fromIPv6b, toIPv4, toIPv6b)
import Control.Applicative (many, some, (<|>))
import Control.Monad (unless, void)
import Data.Maybe (maybe, listToMaybe)

newtype Multiaddr = Multiaddr { parts :: [MultiaddrPart] }
  deriving (Eq, Generic)

instance Show Multiaddr where
  show (Multiaddr m) = concatMap show m

instance Read Multiaddr where
  readsPrec _ = Parser.readP_to_S $ do
    multiParts <- some $ Parser.readS_to_P reads
    many $ Parser.char '/'
    return $ Multiaddr multiParts

data MultiaddrPart = IP4m   { ip4m   :: IPv4 }
                   | IP6m   { ip6m   :: IPv6 }
                   | TCPm   { tcpm   :: Port }
                   | UDPm   { udpm   :: Port }
                   | DCCPm  { dccpm  :: Port }
                   | SCTPm  { sctpm  :: Port }
                   | ONIONm { onionm :: Onion }
                   | P2Pm   { p2pm  :: MHD.MultihashDigest }
                   | UNIXm  { unixm  :: UnixPath }
                   | UTPm
                   | UDTm
                   | QUICm
                   | HTTPm
                   | HTTPSm
                   | WSm
                   | WSSm
                   deriving (Eq, Generic)

instance Show MultiaddrPart where
  show (IP4m i)   = "/ip4/"   ++ show i
  show (IP6m i)   = "/ip6/"   ++ show i
  show (TCPm p)   = "/tcp/"   ++ show p
  show (UDPm p)   = "/udp/"   ++ show p
  show (DCCPm p)  = "/dccp/"  ++ show p
  show (SCTPm p)  = "/sctp/"  ++ show p
  show (ONIONm a) = "/onion/" ++ show a
  show (P2Pm h)   = "/p2p/"   ++ show h
  show (UNIXm p)  = "/unix/"  ++ show p
  show UTPm       = "/utp"
  show UDTm       = "/udt"
  show QUICm      = "/quic"
  show HTTPm      = "/http"
  show HTTPSm     = "/https"
  show WSm        = "/ws"
  show WSSm       = "/wss"

instance Read MultiaddrPart where
  readsPrec _ = Parser.readP_to_S $  parseAddr IP4m "ip4"
                                 <|> parseAddr IP6m "ip6"
                                 <|> parseAddr TCPm "tcp"
                                 <|> parseAddr UDPm "udp"
                                 <|> parseAddr DCCPm "dccp"
                                 <|> parseAddr SCTPm "sctp"
                                 <|> parseAddr ONIONm "onion"
                                 <|> parseAddr P2Pm "ipfs"
                                 <|> parseAddr P2Pm "p2p"
                                 <|> parseAddr UNIXm "unix"
                                 <|> parse     UTPm "utp"
                                 <|> parse     UDTm "udt"
                                 <|> parse     QUICm "quic"
                                 <|> parse     HTTPm "http"
                                 <|> parse     HTTPSm "https"
                                 <|> parse     WSm "ws"
                                 <|> parse     WSSm "wss"

parse :: MultiaddrPart -> String -> Parser.ReadP MultiaddrPart
parse c s = c <$ (protocolHeader s)

parseAddr :: Read a => (a -> MultiaddrPart) -> String -> Parser.ReadP MultiaddrPart
parseAddr c s = c <$> (protocolHeader s *> sep *> protocolAddr)

rangeParse :: Read a => Parser.ReadP a -> Int -> Int -> Parser.ReadP [a]
rangeParse parse min max = foldr1 (Parser.<++) $
  fmap (\n -> Parser.count n $ parse) [max, max-1..min]

sep :: Parser.ReadP String
sep = some $ Parser.char '/'

protocolHeader :: String -> Parser.ReadP String
protocolHeader s = sep *> Parser.string s

protocolAddr :: Read a => Parser.ReadP a
protocolAddr = Parser.readS_to_P reads

instance Read MHD.MultihashDigest where
  readsPrec _ = Parser.readP_to_S $ do
    multihashText <- Parser.munch1 (/= '/')
    case ((MHB.decode MHB.Base58) (BSLazyChar.pack multihashText)) of
      Right bs ->
        case MHD.decode (BSLazyChar.toStrict bs) of
          Right digest -> return digest
          otherwise -> Parser.pfail
      otherwise -> Parser.pfail

newtype Port = Port
  {
    port :: Word16
  }
  deriving (Show, Eq, Generic)

toPort :: (Integral a, Eq a) => a -> Maybe Port
toPort n | n < 1 || n > 65535 = Nothing
         | otherwise          = Just $ Port (fromIntegral n)

fromPort :: Integral a => Port -> a
fromPort p = fromIntegral (port p)

instance Read Port where
  readsPrec _ = Parser.readP_to_S $ do
    port <- fmap read $ rangeParse (Parser.satisfy isDigit) 1 5 :: Parser.ReadP Int
    case toPort port of
      Just p -> return p
      Nothing -> Parser.pfail

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
    case BSBase32.decode onionHash of
      Right onionHashDecoded -> do
        Parser.char ':'
        onionPort <- Parser.readS_to_P reads :: Parser.ReadP Port
        return $ Onion onionHashDecoded onionPort
      otherwise -> Parser.pfail

newtype UnixPath = UnixPath { path :: FilePath } deriving (Show, Eq, Generic)

instance Read UnixPath where
  readsPrec _ = fmap (maybe [] (:[]) . listToMaybe) $ Parser.readP_to_S $ do
    path <- Parser.many1 Parser.get
    Parser.manyTill (Parser.char '/') Parser.eof
    return $ UnixPath $ "/" ++ path

encode = undefined
decode = undefined
