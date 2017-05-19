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
    protocolPrefixStr,
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

protocolPrefixStr :: MultiaddrPart -> String
protocolPrefixStr (IP4m _)   = "ip4"
protocolPrefixStr (IP6m _)   = "ip6"
protocolPrefixStr (TCPm _)   = "tcp"
protocolPrefixStr (UDPm _)   = "udp"
protocolPrefixStr (DCCPm _)  = "dccp"
protocolPrefixStr (SCTPm _)  = "sctp"
protocolPrefixStr (ONIONm _) = "onion"
protocolPrefixStr (IPFSm _)  = "ipfs"
protocolPrefixStr (P2Pm _)   = "p2p"
protocolPrefixStr (UNIXm _)  = "unix"
protocolPrefixStr UTPm       = "utp"
protocolPrefixStr UDTm       = "udt"
protocolPrefixStr QUICm      = "quic"
protocolPrefixStr HTTPm      = "http"
protocolPrefixStr HTTPSm     = "https"
protocolPrefixStr WSm        = "ws"
protocolPrefixStr WSSm       = "wss"

instance Show MultiaddrPart where
  show c@(IP4m a)   = "/" ++ protocolPrefixStr c ++ "/" ++ show a
  show c@(IP6m a)   = "/" ++ protocolPrefixStr c ++ "/" ++ show a
  show c@(TCPm a)   = "/" ++ protocolPrefixStr c ++ "/" ++ show a
  show c@(UDPm a)   = "/" ++ protocolPrefixStr c ++ "/" ++ show a
  show c@(DCCPm a)  = "/" ++ protocolPrefixStr c ++ "/" ++ show a
  show c@(SCTPm a)  = "/" ++ protocolPrefixStr c ++ "/" ++ show a
  show c@(ONIONm a) = "/" ++ protocolPrefixStr c ++ "/" ++ show a
  show c@(IPFSm a)  = "/" ++ protocolPrefixStr c ++ "/" ++ show a
  show c@(P2Pm a)   = "/" ++ protocolPrefixStr c ++ "/" ++ show a
  show c@(UNIXm a)  = "/" ++ protocolPrefixStr c ++ "/" ++ show a
  show c@UTPm       = "/" ++ protocolPrefixStr c
  show c@UDTm       = "/" ++ protocolPrefixStr c
  show c@QUICm      = "/" ++ protocolPrefixStr c
  show c@HTTPm      = "/" ++ protocolPrefixStr c
  show c@HTTPSm     = "/" ++ protocolPrefixStr c
  show c@WSm        = "/" ++ protocolPrefixStr c
  show c@WSSm       = "/" ++ protocolPrefixStr c

instance Read MultiaddrPart where
  readsPrec _ = Parser.readP_to_S  $  parseAddr IP4m
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

parsePrefix :: MultiaddrPart -> Parser.ReadP MultiaddrPart
parsePrefix c = c <$
  (protocolHeader $ protocolPrefixStr $ c)

parseAddr :: Read a => (a -> MultiaddrPart) -> Parser.ReadP MultiaddrPart
parseAddr c = c <$>
  (protocolHeader (protocolPrefixStr $ c undefined) *> sep *> protocolAddr)

sep :: Parser.ReadP String
sep = some $ Parser.char '/'

protocolHeader :: String -> Parser.ReadP String
protocolHeader s = sep *> Parser.string s

protocolAddr :: Read a => Parser.ReadP a
protocolAddr = Parser.readS_to_P reads

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

decodePrefix :: MultiaddrPart -> Get MultiaddrPart
decodePrefix c = c <$ (protocolHeaderBytes $ protocolPrefix c)

decodeAddr :: (BSStrict.ByteString -> MultiaddrPart) -> MultiaddrPartSize -> Get MultiaddrPart
decodeAddr c s = c <$> (protocolHeaderBytes (protocolPrefix $ c undefined) *> protocolAddrBytes s)

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

encapsulate :: Multiaddr -> Multiaddr -> Multiaddr
encapsulate m1 m2 = Multiaddr $ parts m1 ++ parts m2

decapsulate :: Multiaddr -> Multiaddr -> Maybe Multiaddr
decapsulate (Multiaddr p1) (Multiaddr p2)
  | isPrefixOf p1 p2 = Just . Multiaddr . drop (length p1) $ p2
  | otherwise = Nothing

findFirstPart :: MultiaddrPart -> Multiaddr -> Maybe MultiaddrPart
findFirstPart part addr = find
                          (\part -> protocolPrefix part == partIndex)
                          (parts addr)
  where
    partIndex = protocolPrefix part

findLastPart :: MultiaddrPart -> Multiaddr -> Maybe MultiaddrPart
findLastPart part addr = go
                         (\part -> protocolPrefix part == partIndex)
                         (parts addr)
                         Nothing
  where
    partIndex = protocolPrefix part
    go _ [] lastPart = lastPart
    go pred (p:ps) lastPart
      | pred p = go pred ps $ Just p
      | otherwise = go pred ps lastPart

findAllParts :: MultiaddrPart -> Multiaddr -> [MultiaddrPart]
findAllParts part addr = filter
                         (\part -> protocolPrefix part == partIndex)
                         (parts addr)
  where
    partIndex = protocolPrefix part

instance Read MHD.MultihashDigest where
  readsPrec _ = Parser.readP_to_S $ do
    multihashText <- Parser.munch1 (/= '/')
    case ((MHB.decode MHB.Base58) (BSLazyChar.pack multihashText)) of
      Right bs ->
        case MHD.decode (BSLazyChar.toStrict bs) of
          Right digest -> return digest
          otherwise -> Parser.pfail
      otherwise -> Parser.pfail
