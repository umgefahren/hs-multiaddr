{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Data.Multiaddr
  (
    -- * As Multiaddr types
    Multiaddr (..),
    MultiaddrPart (..),
    module Data.Multiaddr.Port,
    module Data.Multiaddr.Onion,
    module Data.Multiaddr.UnixPath,
    -- * As a string
    show,
    read,
    -- * As binary packed format
    module Data.Multiaddr.Encode,
    module Data.Multiaddr.Decode,
    -- * Utilities
    fromPort,
    toPort
    -- encapsulate,
    -- decapsulate,
    -- startsWith
  ) where

import qualified Control.Exception as E
import qualified Text.ParserCombinators.ReadP as Parser

import qualified Data.Multihash.Digest as MHD
import qualified Data.Multihash.Base as MHB

import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Lazy.Char8 as BSLazyChar

import GHC.Generics (Generic)
import Control.Applicative (many, some, (<|>))
import Control.Monad (unless, void)
import Data.IP (IPv4, IPv6)

import Data.Multiaddr.Port
import Data.Multiaddr.Onion
import Data.Multiaddr.UnixPath
import Data.Multiaddr.Encode
import Data.Multiaddr.Decode

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
                   | IPFSm  { ipfsm :: MHD.MultihashDigest }
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
  show (IPFSm h)  = "/ipfs"   ++ show h
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
                                 <|> parseAddr IPFSm "ipfs"
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



