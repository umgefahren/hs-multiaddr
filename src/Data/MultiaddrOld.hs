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
    addrToString,
    stringToAddr,
    -- * As binary packed format
    encode,
    decode,
    -- * Utilities
    protocolHeaderB,
    protocolHeaderS,
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
import Control.Monad (unless, void)
import Control.Arrow (second)
import Data.List (isPrefixOf, find, filter)
import Data.IP (IPv4, IPv6, fromIPv4, fromIPv6b, toIPv4, toIPv6b)
import Data.Bytes.VarInt (VarInt (..))
import Data.Bytes.Get (getByteString, runGetS)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (serialize, deserialize)
import Data.Serialize.Get (Get)

import Data.String

import qualified Data.Multiaddr.Port as Port
import qualified Data.Multiaddr.Onion as Onion
import qualified Data.Multiaddr.UnixPath as UnixPath
import qualified Date.Multiaddr.VarInt as VarInt

newtype Multiaddr = Multiaddr { parts :: [MultiaddrPart] }
  deriving (Eq, Generic)

addrToString :: Multiaddr -> String
addrToString (Multiaddr a) = concatMap partToString a

stringToAddr :: String -> Either String Multiaddr
stringToAddr s =
  case parseMultiaddr s of
    [(m, [])] -> Right m
    otherwise -> Left $ "Error parsing multiaddr " ++ s
  where
      many $ Parser.char '/'
      return $ Multiaddr multiParts

instance IsString Multiaddr where
  fromString s = case stringToAddr s of
    Right m -> m
    Left e  -> error e

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
                   deriving (Show, Eq, Generic)

protocolHeaderB :: MultiaddrPart -> VarInt Int
protocolHeaderB (IP4m _)   = 4
protocolHeaderB (IP6m _)   = 41
protocolHeaderB (TCPm _)   = 6
protocolHeaderB (UDPm _)   = 17
protocolHeaderB (DCCPm _)  = 33
protocolHeaderB (SCTPm _)  = 132
protocolHeaderB (ONIONm _) = 444
protocolHeaderB (IPFSm _)  = 421
protocolHeaderB (P2Pm _)   = 420
protocolHeaderB (UNIXm _)  = 400
protocolHeaderB UTPm       = 302
protocolHeaderB UDTm       = 301
protocolHeaderB QUICm      = 81
protocolHeaderB HTTPm      = 480
protocolHeaderB HTTPSm     = 443
protocolHeaderB WSm        = 477
protocolHeaderB WSSm       = 478

protocolHeaderS :: MultiaddrPart -> String
protocolHeaderS (IP4m _)   = "ip4"
protocolHeaderS (IP6m _)   = "ip6"
protocolHeaderS (TCPm _)   = "tcp"
protocolHeaderS (UDPm _)   = "udp"
protocolHeaderS (DCCPm _)  = "dccp"
protocolHeaderS (SCTPm _)  = "sctp"
protocolHeaderS (ONIONm _) = "onion"
protocolHeaderS (IPFSm _)  = "ipfs"
protocolHeaderS (P2Pm _)   = "p2p"
protocolHeaderS (UNIXm _)  = "unix"
protocolHeaderS UTPm       = "utp"
protocolHeaderS UDTm       = "udt"
protocolHeaderS QUICm      = "quic"
protocolHeaderS HTTPm      = "http"
protocolHeaderS HTTPSm     = "https"
protocolHeaderS WSm        = "ws"
protocolHeaderS WSSm       = "wss"

partToString :: MultiaddrPart -> String
partToString c@(IP4m a)   =
  "/" ++ protocolHeaderS c ++ "/" ++ show a
partToString c@(IP6m a)   =
  "/" ++ protocolHeaderS c ++ "/" ++ show a
partToString c@(TCPm a)   =
  "/" ++ protocolHeaderS c ++ "/" ++ show a
partToString c@(UDPm a)   =
  "/" ++ protocolHeaderS c ++ "/" ++ show a
partToString c@(DCCPm a)  =
  "/" ++ protocolHeaderS c ++ "/" ++ show a
partToString c@(SCTPm a)  =
  "/" ++ protocolHeaderS c ++ "/" ++ show a
partToString c@(ONIONm a) =
  "/" ++ protocolHeaderS c ++ "/" ++ BSBase32
partToString c@(IPFSm a)  =
  "/" ++
  protocolHeaderS c ++
  "/" ++
  (BSLazyChar.unpack $ MHB.encode MHB.Base58 (BSLazy.fromStrict $ MHD.digest a))
partToString c@(P2Pm a)   =
  "/" ++
  protocolHeaderS c ++
  "/" ++
  (BSLazyChar.unpack $ MHB.encode MHB.Base58 (BSLazy.fromStrict $ MHD.digest a))
partToString c@(UNIXm a)  =
  "/" ++ protocolHeaderS c ++ "/" ++ show a
partToString c@UTPm       =
  "/" ++ protocolHeaderS c
partToString c@UDTm       =
  "/" ++ protocolHeaderS c
partToString c@QUICm      =
  "/" ++ protocolHeaderS c
partToString c@HTTPm      =
  "/" ++ protocolHeaderS c
partToString c@HTTPSm     =
  "/" ++ protocolHeaderS c
partToString c@WSm        =
  "/" ++ protocolHeaderS c
partToString c@WSSm       =
  "/" ++ protocolHeaderS c

parsePart :: Parser.ReadP MultiaddrPart
parsePart  =  parseAddr IP4m
          <|> parseAddr IP6m
          <|> parseAddr TCPm
          <|> parseAddr UDPm
          <|> parseAddr DCCPm
          <|> parseAddr SCTPm
          <|> parseAddr ONIONm
          <|> parseAddr IPFSm
          <|> parseAddr P2Pm
          <|> parseAddr UNIXm
          <|> parsePrefix UTPm
          <|> parsePrefix UDTm
          <|> parsePrefix QUICm
          <|> parsePrefix HTTPm
          <|> parsePrefix HTTPSm
          <|> parsePrefix WSm
          <|> parsePrefix WSSm

parseAddr :: Read a => (a -> MultiaddrPart) -> Parser.ReadP MultiaddrPart
parseAddr c = c <$>
  (parseProtHeader (protocolHeaderS $ c undefined) *> parseSep *> parseProtAddr)
  where parseProtAddr = Parser.readS_to_P reads

parsePrefix :: MultiaddrPart -> Parser.ReadP MultiaddrPart
parsePrefix c = c <$
  (parseProtHeader $ protocolHeaderS c)

parseSep :: Parser.ReadP String
parseSep = some $ Parser.char '/'

parseProtHeader :: String -> Parser.ReadP String
parseProtHeader s = parseSep *> Parser.string s

-- ENCODING & DECODING

encode :: Multiaddr -> BSStrict.ByteString
encode (Multiaddr parts) = BSStrict.concat . map encodeAll $ parts

decode :: BSStrict.ByteString -> Either String Multiaddr
decode = runGetS $ fmap Multiaddr $ some decodeAll

-- size in bits
data MultiaddrPartSize = PartSizeVar
                     | PartSize Int
                       deriving (Eq, Show)

encodeAll :: MultiaddrPart -> BSStrict.ByteString
encodeAll c@(IP4m i)   = BSStrict.append (encodePrefix c) (encodeList. fromIPv4 $ i)
encodeAll c@(IP6m i)   = BSStrict.append (encodePrefix c) (encodeList . fromIPv6b $ i)
encodeAll c@(TCPm p)   = BSStrict.append (encodePrefix c) (encodePort p)
encodeAll c@(UDPm p)   = BSStrict.append (encodePrefix c) (encodePort p)
encodeAll c@(DCCPm p)  = BSStrict.append (encodePrefix c) (encodePort p)
encodeAll c@(SCTPm p)  = BSStrict.append (encodePrefix c) (encodePort p)
encodeAll c@(ONIONm o) = BSStrict.concat [(encodePrefix c), (encodeAddr $ onionHash o), (encodePort $ onionPort o)]
encodeAll c@(IPFSm h)  = BSStrict.append (encodePrefix c) (encodeAddr $ MHD.digest h)
encodeAll c@(P2Pm h)   = BSStrict.append (encodePrefix c) (encodeAddr $ MHD.digest h)
encodeAll c@(UNIXm p)  = BSStrict.append (encodePrefix c) (encodeAddr $ BSStrictChar.pack $ path p)
encodeAll c@UTPm       = encodePrefix c
encodeAll c@UDTm       = encodePrefix c
encodeAll c@QUICm      = encodePrefix c
encodeAll c@HTTPm      = encodePrefix c
encodeAll c@HTTPSm     = encodePrefix c
encodeAll c@WSm        = encodePrefix c
encodeAll c@WSSm       = encodePrefix c

encodePrefix :: MultiaddrPart -> BSStrict.ByteString
encodePrefix = encodeVarInt . protocolHeaderB

encodeAddr :: BSStrict.ByteString -> BSStrict.ByteString
encodeAddr b = BSStrict.append (encodeVarInt . fromIntegral . BSStrict.length $ b) b

encodeVarInt :: VarInt Int -> BSStrict.ByteString
encodeVarInt = runPutS . serialize

encodeList :: Integral a => [a] -> BSStrict.ByteString
encodeList = BSStrict.pack . map fromIntegral

-- fixed-width types passed to serialize gets serialised as big endian
-- multiaddr ports must be encoded big-endian
encodePort :: Port -> BSStrict.ByteString
encodePort p = (runPutS . serialize) $ port p

decodeAll :: Get MultiaddrPart
decodeAll  =  decodeAddr
               (IP4m . toIPv4 . decodeList)
               (PartSize 32)
         <|> decodeAddr
               (IP6m . toIPv6b . decodeList)
               (PartSize 128)
         <|> decodeAddr
               (TCPm . decodePort)
               (PartSize 16)
         <|> decodeAddr
               (UDPm . decodePort)
               (PartSize 16)
         <|> decodeAddr
               (DCCPm . decodePort)
               (PartSize 16)
         <|> decodeAddr
               (SCTPm . decodePort)
               (PartSize 16)
         <|> decodeAddr
               (ONIONm . uncurry Onion . second decodePort . BSStrict.splitAt 10)
               (PartSize 96)
         <|> decodeAddr
               (IPFSm . (either error id) . MHD.decode)
               PartSizeVar
         <|> decodeAddr
               (P2Pm . (either error id) . MHD.decode)
               PartSizeVar
         <|> decodeAddr
               (UNIXm . UnixPath . BSStrictChar.unpack)
               PartSizeVar
         <|> decodePrefix UTPm
         <|> decodePrefix UDTm
         <|> decodePrefix QUICm
         <|> decodePrefix HTTPSm
         <|> decodePrefix WSm
         <|> decodePrefix WSSm

decodeAddr :: (BSStrict.ByteString -> MultiaddrPart) -> MultiaddrPartSize -> Get MultiaddrPart
decodeAddr c s = c <$>
  (parseProtHeaderB (protocolHeaderB $ c undefined) *> parseProtAddrB s)
  where
    parseProtAddrB PartSizeVar  = getByteString . fromIntegral =<<
      (deserialize :: Get (VarInt Int))
    parseProtAddrB (PartSize n) = getByteString $ fromIntegral $ div n 8

decodePrefix :: MultiaddrPart -> Get MultiaddrPart
decodePrefix c = c <$
  (parseProtHeaderB $ protocolHeaderB c)

parseProtHeaderB :: VarInt Int -> Get ()
parseProtHeaderB i = do
  p <- deserialize :: Get (VarInt Int)
  unless (i == p) $ fail "Wrong protocol index"

decodeList :: Integral a => BSStrict.ByteString -> [a]
decodeList = map fromIntegral . BSStrict.unpack

decodePort :: BSStrict.ByteString -> Port
decodePort = either error Port . runGetS deserialize

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

instance Read MHD.MultihashDigest where
  readsPrec _ = Parser.readP_to_S $ do
    multihashText <- Parser.munch1 (/= '/')
    case ((MHB.decode MHB.Base58) (BSLazyChar.pack multihashText)) of
      Right bs ->
        case MHD.decode (BSLazyChar.toStrict bs) of
          Right digest -> return digest
          otherwise -> Parser.pfail
      otherwise -> Parser.pfail
