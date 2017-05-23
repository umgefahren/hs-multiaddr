{-# LANGUAGE DeriveGeneric #-}

module Data.MultiaddrPart
  (
    MultiaddrPart (..),
    toString,
    parse,
    encode,
    parseB,
    partHdrS,
    partHdrB
  ) where

import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict
import qualified Data.Multiaddr.IPFS as IPFS
import qualified Data.Multiaddr.VarInt as VarInt
import qualified Data.Multiaddr.IPv4 as IPv4
import qualified Data.Multiaddr.IPv6 as IPv6
import qualified Data.Multiaddr.Port as Port
import qualified Data.Multiaddr.Onion as Onion
import qualified Data.Multiaddr.UnixPath as UnixPath

import GHC.Generics (Generic)
import Control.Applicative (some, (<|>))
import Control.Monad (unless)
import Data.Serialize.Get (Get)

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

toString :: MultiaddrPart -> String
toString p = concat ["/", partHdrS p, go p]
  where
    go (IP4m i)   = "/" ++ IPv4.toString i
    go (IP6m i)   = "/" ++ IPv6.toString i
    go (TCPm p)   = "/" ++ Port.toString p
    go (UDPm p)   = "/" ++ Port.toString p
    go (DCCPm p)  = "/" ++ Port.toString p
    go (SCTPm p)  = "/" ++ Port.toString p
    go (ONIONm o) = "/" ++ Onion.toString o
    go (IPFSm h)  = "/" ++ IPFS.toString h
    go (P2Pm h)   = "/" ++ IPFS.toString h
    go (UNIXm p)  = "/" ++ UnixPath.toString p
    go UTPm       = ""
    go UDTm       = ""
    go QUICm      = ""
    go HTTPm      = ""
    go HTTPSm     = ""
    go WSm        = ""
    go WSSm       = ""

parse :: Parser.ReadP MultiaddrPart
parse =
  (IP4m <$> (parseHdr (IP4m undefined) *> parseSep *> IPv4.parse)) <|>
  (IP6m <$> (parseHdr (IP6m undefined) *> parseSep *> IPv6.parse)) <|>
  (TCPm <$> (parseHdr (TCPm undefined) *> parseSep *> Port.parse)) <|>
  (UDPm <$> (parseHdr (UDPm undefined) *> parseSep *> Port.parse)) <|>
  (DCCPm <$> (parseHdr (DCCPm undefined) *> parseSep *> Port.parse)) <|>
  (SCTPm <$> (parseHdr (SCTPm undefined) *> parseSep *> Port.parse)) <|>
  (ONIONm <$> (parseHdr (ONIONm undefined) *> parseSep *> Onion.parse)) <|>
  (IPFSm <$> (parseHdr (IPFSm undefined) *> parseSep *> IPFS.parse)) <|>
  (P2Pm <$> (parseHdr (P2Pm undefined) *> parseSep *> IPFS.parse)) <|>
  (UNIXm <$> (parseHdr (UNIXm undefined) *> parseSep *> UnixPath.parse)) <|>
  (parseHdr UTPm) <|>
  (parseHdr UDTm) <|>
  (parseHdr QUICm) <|>
  (parseHdr HTTPm) <|>
  (parseHdr HTTPSm) <|>
  (parseHdr WSm) <|>
  (parseHdr WSSm)
  where
    parseHdr c = c <$ (parseSep *> Parser.string (partHdrS c))
    parseSep = some $ Parser.char '/'

encode :: MultiaddrPart -> BSStrict.ByteString
encode p = BSStrict.append (VarInt.encode $ partHdrB p) (go p)
  where
    go (IP4m i)   = IPv4.encode i
    go (IP6m i)   = IPv6.encode i
    go (TCPm p)   = Port.encode p
    go (UDPm p)   = Port.encode p
    go (DCCPm p)  = Port.encode p
    go (SCTPm p)  = Port.encode p
    go (ONIONm o) = Onion.encode o
    go (IPFSm h)  = IPFS.encode h
    go (P2Pm h)   = IPFS.encode h
    go (UNIXm p)  = UnixPath.encode p
    go UTPm       = BSStrict.empty
    go UDTm       = BSStrict.empty
    go QUICm      = BSStrict.empty
    go HTTPm      = BSStrict.empty
    go HTTPSm     = BSStrict.empty
    go WSm        = BSStrict.empty
    go WSSm       = BSStrict.empty

parseB :: Get MultiaddrPart
parseB =
  (IP4m <$> (parseHdrB (IP4m undefined) *> IPv4.parseB)) <|>
  (IP6m <$> (parseHdrB (IP6m undefined) *> IPv6.parseB)) <|>
  (TCPm <$> (parseHdrB (TCPm undefined) *> Port.parseB)) <|>
  (UDPm <$> (parseHdrB (UDPm undefined) *> Port.parseB)) <|>
  (DCCPm <$> (parseHdrB (DCCPm undefined) *> Port.parseB)) <|>
  (SCTPm <$> (parseHdrB (SCTPm undefined) *> Port.parseB)) <|>
  (ONIONm <$> (parseHdrB (ONIONm undefined) *> Onion.parseB)) <|>
  (IPFSm <$> (parseHdrB (IPFSm undefined) *> IPFS.parseB)) <|>
  (P2Pm <$> (parseHdrB (P2Pm undefined) *> IPFS.parseB)) <|>
  (UNIXm <$> (parseHdrB (UNIXm undefined) *> UnixPath.parseB)) <|>
  (parseHdrB UTPm) <|>
  (parseHdrB UDTm) <|>
  (parseHdrB QUICm) <|>
  (parseHdrB HTTPm) <|>
  (parseHdrB HTTPSm) <|>
  (parseHdrB WSm) <|>
  (parseHdrB WSSm)
  where
    parseHdrB c = c <$ do
      i <- VarInt.decode
      unless (i == partHdrB c) $ fail "Wrong protocol index"
