{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module Multiaddr
  (
    -- * As Multiaddr types
    Multiaddr (..),
    MultiaddrPart (..),
    -- * As a string
    show,
    read,
    -- * As binary packed format
    -- encode,
    -- decode,
    -- * Utilities
    -- encapsulate,
    -- decapsulate,
    -- startsWith
  ) where

-- make my types generic
import GHC.Generics (Generic)

-- bring in the combinators and parser
import Control.Applicative (many, some, (<|>))
import Control.Monad (unless, void)
import Text.ParserCombinators.ReadP (ReadP,
                                     char,
                                     string,
                                     count,
                                     satisfy,
                                     readP_to_S,
                                     readS_to_P)

-- types for the addresses
import Data.IP (IPv4, IPv6, fromIPv4, fromIPv6b, toIPv4, toIPv6b)
import Data.Char (isAlphaNum)
import Data.Word (Word16)
import Data.Text (Text, pack)
import System.FilePath (FilePath)

-- Multiaddr is a record wrapping a list of MultiaddrParts
-- a Multiaddr here is a typed Multiaddr within Haskell
-- in the outside world, there are 2 kinds of Multiaddr
-- a textual human readable version
-- a binary encoded version
-- this library exposes the typed Multiaddr
-- while allowing Multiaddr to be created from
-- human readable text or binary encoded string
newtype Multiaddr = Multiaddr { parts :: [MultiaddrPart] }
  deriving (Eq, Generic)

-- Multiaddr can be showed
instance Show Multiaddr where
  show (Multiaddr m) = concatMap show m

-- Multiaddr can be read
-- Precedence is not important
-- reads is generic, it reads a MultiaddrPart
-- for every part, we read 0 or more slashes as a separator
instance Read Multiaddr where
  readsPrec _ = readP_to_S $ do
    multiParts <- some $ readS_to_P reads
    many $ char '/'
    return $ Multiaddr multiParts

-- UNIX works like this /ip4/10.0.0.1/unix/..... the rest of the path is consider the FILE PATH

data MultiaddrPart = IP4   { ip4   :: IPv4 }
                   | IP6   { ip6   :: IPv6 }
                   | TCP   { tcp   :: Word16 }
                   | UDP   { udp   :: Word16 }
                   | DCCP  { dccp  :: Word16 }
                   | SCTP  { sctp  :: Word16 }
                   | ONION { onion :: Onion }
                   | IPFS  { ipfs  :: Multihash }
                   | UNIX  { unix  :: FilePath }
                   | UTP
                   | UDT
                   | QUIC
                   | HTTP
                   | HTTPS
                   | WebSockets
                   | WebSocketsS
                   deriving (Eq, Generic)

instance Show MultiaddrPart where
  show (IP4  i)    = "/ip4/"   ++ show i
  show (IP6  i)    = "/ip6/"   ++ show i
  show (TCP  p)    = "/tcp/"   ++ show p
  show (UDP  p)    = "/udp/"   ++ show p
  show (DCCP p)    = "/dccp/"  ++ show p
  show (SCTP p)    = "/sctp/"  ++ show p
  show (ONION h)   = "/onion/" ++ show h
  show (IPFS h)    = "/ipfs/"  ++ show h
  show (UNIX p)    = "/unix/"  ++ show p
  show UTP         = "/utp"
  show UDT         = "/udt"
  show QUIC        = "/quic"
  show HTTP        = "/http"
  show HTTPS       = "/https"
  show WebSockets  = "/ws"
  show WebSocketsS = "/wss"

-- a port can be 0  to 65,536
-- which means or character length is up to
-- not exactly
-- although in some cases we can go to...
-- an IPv6 can be
-- fixed length here doesn't quite make sense
-- because it the readS_to_P is generic and matches against the internal type
-- if the internal type is readable, then it's done

instance Read MultiaddrPart where
  readsPrec _ = readP_to_S $  parseAddr IP4 "ip4"
                          <|> parseAddr IP6 "ip6"
                          <|> parseAddr TCP "tcp"
                          <|> parseAddr UDP "udp"
                          <|> parseAddr DCCP "dccp"
                          <|> parseAddr SCTP "sctp"
                          <|> parseAddr ONION "onion" -- test readability
                          <|> parseAddr IPFS "ipfs" -- test readability
                          <|> parseAddr UNIX "unix" -- test readability
                          <|> parse     UTP "utp"
                          <|> parse     UDT "udt"
                          <|> parse     QUIC "quic"
                          <|> parse     HTTP "http"
                          <|> parse     HTTPS "https"
                          <|> parse     WebSockets "ws"
                          <|> parse     WebSocketsS "wss"

parse :: Read a => (a -> MultiaddrPart) -> String -> ReadP MultiaddrPart
parse c s = c <$ (protocolHeader s)

parseAddr :: Read a => (a -> MultiaddrPart) -> String -> ReadP MultiaddrPart
parseAddr c s = c <$> (protocolHeader s *> sep *> protocolAddr)

protocolHeader :: String -> ReadP String
protocolHeader s = sep *> string s

sep :: ReadP String
sep = some $ char '/'

protocolAddr :: Read a => ReadP a
protocolAddr = readS_to_P reads

-- for the purposes of multiaddr port
-- the ports here are only between 1 and 65535
-- anything above or beyond these 2 numbers are disallowed
-- because these represent destination ports
-- port 0 does not exist
newtype Port = Port {
  port :: Word16
  } deriving (Show, Eq, Generic)

-- recurse from 5
-- this is weird!??

instance Read Port where
  readsPrec _ = readP_to_S $ do
    -- should limit the size of digits being read to be a max of 65536
    -- however I'm not sure how to do count being max
    -- count is specified exactly atm
    port <- readS_to_P reads :: ReadP Int
    if port > 0 && port < 65536 then
      return $ Port (fromIntegral port)
    else
      pfail

data Onion = Onion {
  onionHash :: Text,
  onionPort :: Word16
  } deriving (Show, Eq, Generic)

-- Multiformats Onion is like this
-- abcdef123456:123
-- apparently the port number must exist
-- 16 alphanumeric
-- so Word16 as well otherwise you can type the Port to be beyond 1 to 65,536
instance Read Onion where
  readsPrec _ = readP_to_S $ do
    onionHash <- count 16 $ satisfy isAlphaNum
    char ':'
    onionPort <- readS_to_P reads :: ReadP Word16
    return $ Onion (pack onionHash) onionPort
