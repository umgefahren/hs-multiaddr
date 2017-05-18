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
    encode,
    decode,
    -- * Utilities
    protocolPrefix,
    -- encapsulate,
    -- decapsulate,
    -- startsWith
  ) where

import qualified Control.Exception as E
import qualified Text.ParserCombinators.ReadP as Parser
import qualified Data.ByteString as BSStrict
import qualified Data.ByteString.Char8 as BSStrictChar
import qualified Data.ByteString.Lazy.Char8 as BSLazyChar
import qualified Data.Multihash.Digest as MHD
import qualified Data.Multihash.Base as MHB

import GHC.Generics (Generic)
import Control.Applicative (many, some, (<|>))
import Control.Monad (unless, void)
import Control.Arrow (second)
import Data.IP (IPv4, IPv6, fromIPv4, fromIPv6b, toIPv4, toIPv6b)
import Data.Bytes.VarInt (VarInt (..))
import Data.Bytes.Get (getByteString, runGetS)
import Data.Bytes.Put (runPutS)
import Data.Bytes.Serial (serialize, deserialize)
import Data.Serialize.Get (Get)

import Data.Multiaddr.Port
import Data.Multiaddr.Onion
import Data.Multiaddr.UnixPath

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
  readsPrec _ = Parser.readP_to_S  $  parseAddr IP4m "ip4"
                                  <|> parseAddr IP6m "ip6"
                                  <|> parseAddr TCPm "tcp"
                                  <|> parseAddr UDPm "udp"
                                  <|> parseAddr DCCPm "dccp"
                                  <|> parseAddr SCTPm "sctp"
                                  <|> parseAddr ONIONm "onion"
                                  <|> parseAddr IPFSm "ipfs"
                                  <|> parseAddr P2Pm "p2p"
                                  <|> parseAddr UNIXm "unix"
                                  <|> parsePrefix UTPm "utp"
                                  <|> parsePrefix UDTm "udt"
                                  <|> parsePrefix QUICm "quic"
                                  <|> parsePrefix HTTPm "http"
                                  <|> parsePrefix HTTPSm "https"
                                  <|> parsePrefix WSm "ws"
                                  <|> parsePrefix WSSm "wss"

parsePrefix :: MultiaddrPart -> String -> Parser.ReadP MultiaddrPart
parsePrefix c s = c <$ (protocolHeader s)

parseAddr :: Read a => (a -> MultiaddrPart) -> String -> Parser.ReadP MultiaddrPart
parseAddr c s = c <$> (protocolHeader s *> sep *> protocolAddr)

sep :: Parser.ReadP String
sep = some $ Parser.char '/'

protocolHeader :: String -> Parser.ReadP String
protocolHeader s = sep *> Parser.string s

protocolAddr :: Read a => Parser.ReadP a
protocolAddr = Parser.readS_to_P reads

protocolPrefix :: MultiaddrPart -> VarInt Int
protocolPrefix (IP4m _)   = 4
protocolPrefix (IP6m _)   = 41
protocolPrefix (TCPm _)   = 6
protocolPrefix (UDPm _)   = 17
protocolPrefix (DCCPm _)  = 33
protocolPrefix (SCTPm _)  = 132
protocolPrefix (ONIONm _) = 444
protocolPrefix (IPFSm _)  = 421
protocolPrefix (P2Pm _)   = 420
protocolPrefix (UNIXm _)  = 400
protocolPrefix UTPm       = 302
protocolPrefix UDTm       = 301
protocolPrefix QUICm      = 81
protocolPrefix HTTPm      = 480
protocolPrefix HTTPSm     = 443
protocolPrefix WSm        = 477
protocolPrefix WSSm       = 478

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
encodePrefix = encodeVarInt . protocolPrefix

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
               (protocolPrefix $ IP4m undefined)
               (PartSize 32)
         <|> decodeAddr
               (IP6m . toIPv6b . decodeList)
               (protocolPrefix $ IP6m undefined)
               (PartSize 128)
         <|> decodeAddr
               (TCPm . decodePort)
               (protocolPrefix $ TCPm undefined)
               (PartSize 16)
         <|> decodeAddr
               (UDPm . decodePort)
               (protocolPrefix $ UDPm undefined)
               (PartSize 16)
         <|> decodeAddr
               (DCCPm . decodePort)
               (protocolPrefix $ DCCPm undefined)
               (PartSize 16)
         <|> decodeAddr
               (SCTPm . decodePort)
               (protocolPrefix $ SCTPm undefined)
               (PartSize 16)
         <|> decodeAddr
               (ONIONm . uncurry Onion . second decodePort . BSStrict.splitAt 10)
               (protocolPrefix $ ONIONm undefined)
               (PartSize 96)
         <|> decodeAddr
               (IPFSm . (either error id) . MHD.decode)
               (protocolPrefix $ IPFSm undefined)
               PartSizeVar
         <|> decodeAddr
               (P2Pm . (either error id) . MHD.decode)
               (protocolPrefix $ P2Pm undefined)
               PartSizeVar
         <|> decodeAddr
               (UNIXm . UnixPath . BSStrictChar.unpack)
               (protocolPrefix $ UNIXm undefined)
               PartSizeVar
         <|> decodePrefix UTPm (protocolPrefix UTPm)
         <|> decodePrefix UDTm (protocolPrefix UDTm)
         <|> decodePrefix QUICm (protocolPrefix HTTPm)
         <|> decodePrefix HTTPSm (protocolPrefix HTTPSm)
         <|> decodePrefix WSm (protocolPrefix WSm)
         <|> decodePrefix WSSm (protocolPrefix WSSm)

decodePrefix :: MultiaddrPart -> VarInt Int -> Get MultiaddrPart
decodePrefix c i = c <$ (protocolHeaderBytes i)

decodeAddr :: (BSStrict.ByteString -> MultiaddrPart) -> VarInt Int -> MultiaddrPartSize -> Get MultiaddrPart
decodeAddr c i s = c <$> (protocolHeaderBytes i *> protocolAddrBytes s)

protocolHeaderBytes :: VarInt Int -> Get ()
protocolHeaderBytes i = do
  p <- deserialize :: Get (VarInt Int)
  unless (i == p) $ fail "Wrong protocol index"

protocolAddrBytes :: MultiaddrPartSize -> Get BSStrict.ByteString
protocolAddrBytes PartSizeVar = getVarByteString
protocolAddrBytes (PartSize n) = getByteString $ fromIntegral $ div n 8

getVarByteString :: Get BSStrict.ByteString
getVarByteString = getByteString . fromIntegral =<< (deserialize :: Get (VarInt Int))

decodeList :: Integral a => BSStrict.ByteString -> [a]
decodeList = map fromIntegral . BSStrict.unpack

decodePort :: BSStrict.ByteString -> Port
decodePort = either error Port . runGetS deserialize

instance Read MHD.MultihashDigest where
  readsPrec _ = Parser.readP_to_S $ do
    multihashText <- Parser.munch1 (/= '/')
    case ((MHB.decode MHB.Base58) (BSLazyChar.pack multihashText)) of
      Right bs ->
        case MHD.decode (BSLazyChar.toStrict bs) of
          Right digest -> return digest
          otherwise -> Parser.pfail
      otherwise -> Parser.pfail
