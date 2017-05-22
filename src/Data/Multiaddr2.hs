{-# LANGUAGE DeriveGeneric #-}

module Data.Multiaddr
  (
    -- * As Multiaddr types
    Multiaddr (..),
    MultiaddrPart (..),
    -- * As a string
    toString,
    toMultiaddr,
    parse,
    -- * As binary packed format
    encode,
    decode,
    parseB,
    -- * Utilities
    partHdrS,
    partHdrB,
    encapsulate,
    decapsulate,
    findFirstPart,
    findLastPart,
    findAllParts
  ) where

import qualified Control.Exception as E
import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Char8 as BSStrictChar
import qualified Data.ByteString.Lazy as BSLazy
import qualified Data.ByteString.Lazy.Char8 as BSLazyChar
import qualified Data.Multihash.Digest as MHD
import qualified Data.Multihash.Base as MHB

import GHC.Generics (Generic)
import Control.Applicative (many, some, (<|>))
import Control.Monad (unless)
import Data.Bytes.Get (runGetS)
import Data.Bytes.Put (runPutS)
import Data.List (isPrefixOf, find, filter)

import qualified Data.Multiaddr.IPFS as IPFS
import qualified Data.Multiaddr.IPv4 as IPv4
import qualified Data.Multiaddr.IPv6 as IPv6
import qualified Data.Multiaddr.Port as Port
import qualified Data.Multiaddr.Onion as Onion
import qualified Data.Multiaddr.UnixPath as UnixPath
import qualified Data.Multiaddr.VarInt as VarInt

import Data.String

newtype Multiaddr = Multiaddr { parts :: [MultiaddrPart] }
  deriving (Show, Eq, Generic)

instance IsString Multiaddr where
  fromString s = case toMultiaddr s of
    Right m -> m
    Left e  -> error e

toString :: Multiaddr -> String
toString (Multiaddr p) = concatMap partToString p

toMultiaddr :: String -> Either String Multiaddr
toMultiaddr s =
  case Parser.readP_to_S parse s of
    [(m, [])] -> Right m
    otherwise -> Left $ "Error parsing multiaddr " ++ s

parse :: Parser.ReadP Multiaddr
parse = do
  multiParts <- some parsePart
  many $ Parser.char '/'
  return $ Multiaddr multiParts

encode :: Multiaddr -> BSStrict.ByteString
encode (Multiaddr p) = BSStrict.concat . map encodePart $ p

decode :: BSStrict.ByteString -> Either String Multiaddr
decode = runGetS $ fmap Multiaddr parseB

parseB :: Get Multiaddr
parseB = some parsePartB

data MultiaddrPart = IP4m   { ip4m   :: IPv4.IPv4 }
                   | IP6m   { ip6m   :: IPv6.IPv6 }
                   | TCPm   { tcpm   :: Port.Port }
                   | UDPm   { udpm   :: Port.Port }
                   | DCCPm  { dccpm  :: Port.Port }
                   | SCTPm  { sctpm  :: Port.Port }
                   | ONIONm { onionm :: Onion.Onion }
                   | IPFSm  { ipfsm :: IPFS.MultihashDigest }
                   | P2Pm   { p2pm  :: IPFS.MultihashDigest }
                   | UNIXm  { unixm  :: UnixPath.UnixPath }
                   | UTPm
                   | UDTm
                   | QUICm
                   | HTTPm
                   | HTTPSm
                   | WSm
                   | WSSm
                   deriving (Show, Eq, Generic)

partHdrB :: MultiaddrPart -> VarInt.VarInt Int
partHdrB (IP4m _)   = 4
partHdrB (IP6m _)   = 41
partHdrB (TCPm _)   = 6
partHdrB (UDPm _)   = 17
partHdrB (DCCPm _)  = 33
partHdrB (SCTPm _)  = 132
partHdrB (ONIONm _) = 444
partHdrB (IPFSm _)  = 421
partHdrB (P2Pm _)   = 420
partHdrB (UNIXm _)  = 400
partHdrB UTPm       = 302
partHdrB UDTm       = 301
partHdrB QUICm      = 81
partHdrB HTTPm      = 480
partHdrB HTTPSm     = 443
partHdrB WSm        = 477
partHdrB WSSm       = 478

partHdrS :: MultiaddrPart -> String
partHdrS (IP4m _)   = "ip4"
partHdrS (IP6m _)   = "ip6"
partHdrS (TCPm _)   = "tcp"
partHdrS (UDPm _)   = "udp"
partHdrS (DCCPm _)  = "dccp"
partHdrS (SCTPm _)  = "sctp"
partHdrS (ONIONm _) = "onion"
partHdrS (IPFSm _)  = "ipfs"
partHdrS (P2Pm _)   = "p2p"
partHdrS (UNIXm _)  = "unix"
partHdrS UTPm       = "utp"
partHdrS UDTm       = "udt"
partHdrS QUICm      = "quic"
partHdrS HTTPm      = "http"
partHdrS HTTPSm     = "https"
partHdrS WSm        = "ws"
partHdrS WSSm       = "wss"

partToString :: MultiaddrPart -> String
partToString p = concat ["/", partHdrS p, go p]
  where
    go (IP4m i)   = IPv4.toString i
    go (IP6m i)   = IPv6.toString i
    go (TCPm p)   = Port.toString p
    go (UDPm p)   = Port.toString p
    go (DCCPm p)  = Port.toString p
    go (SCTPm p)  = Port.toString p
    go (ONIONm o) = Onion.toString o
    go (IPFSm h)  = IPFS.toString h
    go (P2Pm h)   = IPFS.toString h
    go (UNIXm p)  = UnixPath.toString p
    go UTPm       = ""
    go UDTm       = ""
    go QUICm      = ""
    go HTTPm      = ""
    go HTTPSm     = ""
    go WSm        = ""
    go WSSm       = ""

parsePart :: Parser.ReadP MultiaddrPart
parsePart  =  IP4m <$> (parseHdr (IP4m undefined) *> parseSep *> IPv4.parse)
          <|> IP6m <$> (parseHdr (IP6m undefined) *> parseSep *> IPv6.parse)
          <|> TCPm <$> (parseHdr (TCPm undefined) *> parseSep *> Port.parse)
          <|> UDPm <$> (parseHdr (UDPm undefined) *> parseSep *> Port.parse)
          <|> DCCPm <$> (parseHdr (DCCPm undefined) *> parseSep *> Port.parse)
          <|> SCTPm <$> (parseHdr (SCTPm undefined) *> parseSep *> Port.parse)
          <|> ONIONm <$> (parseHdr (ONIONm undefined) *> parseSep *> Onion.parse)
          <|> UNIXm <$> (parseHdr (UNIXm undefined) *> parseSep *> UnixPath.parse)
          <|> parseHdr UTPm
          <|> parseHdr UDTm
          <|> parseHdr QUICm
          <|> parseHdr HTTPm
          <|> parseHdr HTTPSm
          <|> parseHdr WSm
          <|> parseHdr WSSm
  where
    parseHdr c = c <$ (parseSep *> Parser.string (partHdrS c))
    parseSep = some $ Parser.char '/'

encodePart :: MultiaddrPart -> BSStrict.ByteString
encodePart p = BSStrict.append (VarInt.encode $ partHdrB p) (go p)
  where
    go (IP4m i)   = IPv4.encode i
    go (IP6m i)   = IPv6.encode i
    go (TCPm p)   = Port.encode p
    go (UDPm p)   = Port.encode p
    go (DCCPm p)  = Port.encode p
    go (SCTPm p)  = Port.encode p
    go (ONIONm o) = Onion.encode o
    go (UNIXm p)  = UnixPath.encode p
    go UTPm       = BSStrict.empty
    go UDTm       = BSStrict.empty
    go QUICm      = BSStrict.empty
    go HTTPm      = BSStrict.empty
    go HTTPSm     = BSStrict.empty
    go WSm        = BSStrict.empty
    go WSSm       = BSStrict.empty

parsePartB :: Get MultiaddrPart
parsePartB  =  IP4m <$> (parseHdrB (IP4m undefined) *> IPv4.parseB)
           <|> IP6m <$> (parseHdrB (IP6m undefined) *> IPv6.parseB)
           <|> TCPm <$> (parseHdrB (TCPm undefined) *> Port.parseB)
           <|> UDPm <$> (parseHdrB (UDPm undefined) *> Port.parseB)
           <|> DCCPm <$> (parseHdrB (DCCPm undefined) *> Port.parseB)
           <|> SCTPm <$> (parseHdrB (SCTPm undefined) *> Port.parseB)
           <|> ONIONm <$> (parseHdrB (ONIONm undefined) *> Onion.parseB
           <|> UNIXm <$> (parseHdrB (UNIXm undefined) *> UnixPath.parseB)
           <|> parseHdrB UTPm
           <|> parseHdrB UDTm
           <|> parseHdrB QUICm
           <|> parseHdrB HTTPm
           <|> parseHdrB HTTPSm
           <|> parseHdrB WSm
           <|> parseHdrB WSSm
  where
    parseHdrB c = c <$ do
      i <- VarInt.decode
      unless (i == partHdrB c) $ fail "Wrong protocol index"

-- first address encapsulates the second address
encapsulate :: Multiaddr -> Multiaddr -> Multiaddr
encapsulate m1 m2 = Multiaddr $ parts m1 ++ parts m2

-- first address is prefix removed from the second address
decapsulate :: Multiaddr -> Multiaddr -> Maybe Multiaddr
decapsulate (Multiaddr p1) (Multiaddr p2)
  | isPrefixOf p1 p2 = Just . Multiaddr . drop (length p1) $ p2
  | otherwise = Nothing

findFirstPart :: MultiaddrPart -> Multiaddr -> Maybe MultiaddrPart
findFirstPart part addr = find
                          (\part -> protocolHeaderB part == partIndex)
                          (parts addr)
  where
    partIndex = protocolHeaderB part

findLastPart :: MultiaddrPart -> Multiaddr -> Maybe MultiaddrPart
findLastPart part addr = go
                         (\part -> protocolHeaderB part == partIndex)
                         (parts addr)
                         Nothing
  where
    partIndex = protocolHeaderB part
    go _ [] lastPart = lastPart
    go pred (p:ps) lastPart
      | pred p = go pred ps $ Just p
      | otherwise = go pred ps lastPart

findAllParts :: MultiaddrPart -> Multiaddr -> [MultiaddrPart]
findAllParts part addr = filter
                         (\part -> protocolHeaderB part == partIndex)
                         (parts addr)
  where
    partIndex = protocolHeaderB part
