import qualified Data.ByteString.Char8 as BSStrictChar
import qualified Data.ByteString.Lazy.Char8 as BSLazyChar
import qualified Codec.Binary.Base32 as Base32
import qualified Data.Multihash.Digest as MHD
import qualified Data.Multihash.Base as MHB

import Data.IP (IPv4, IPv6, fromIPv4, fromIPv6, toIPv4, toIPv6)
import Data.Maybe (fromJust)
import Data.Char (toUpper)
import Data.Either.Unwrap (fromRight)
import Control.Exception (evaluate)

import Test.Hspec
import Data.Multiaddr

parseFail str = it ("should not parse `" ++ str ++ "`") $ do
  evaluate (read str :: Multiaddr) `shouldThrow` anyException

parseSucceed str addr = it ("should parse `" ++ str ++ "`") $ do
  (read str :: Multiaddr) `shouldBe` addr

toIPFSm hash = IPFSm
              $ fromRight
              $ MHD.decode
              $ BSLazyChar.toStrict
              $ fromRight
              $ MHB.decode MHB.Base58
              $ BSLazyChar.pack hash

toP2Pm hash = P2Pm
              $ fromRight
              $ MHD.decode
              $ BSLazyChar.toStrict
              $ fromRight
              $ MHB.decode MHB.Base58
              $ BSLazyChar.pack hash

toONIONm hash port = ONIONm $
                       Onion
                       (fromRight $
                         Base32.decode $
                           BSStrictChar.pack $ map toUpper hash)
                       (fromJust $ toPort port)

main :: IO ()
main = hspec $ do
  describe "Multiaddr read" $ do
    parseFail "/ip4"
    parseFail "/ip4::1"
    parseFail "/ip4/fdpsofodsajfdoisa"
    parseFail "/ip6"
    parseFail "/udp"
    parseFail "/tcp"
    parseFail "/sctp"
    parseFail "/udp/65536"
    parseFail "/tcp/65536"
    parseFail "/quic/65536"
    parseFail "/onion/9imaq4ygg2iegci7:80"
    parseFail "/onion/aaimaq4ygg2iegci7:80"
    parseFail "/onion/timaq4ygg2iegci7:0"
    parseFail "/onion/timaq4ygg2iegci7:-1"
    parseFail "/onion/timaq4ygg2iegci7"
    parseFail "/onion/timaq4ygg2iegci@:666"
    parseFail "/udp/1234/sctp"
    parseFail "/udp/1234/udt/1234"
    parseFail "/udp/1234/utp/1234"
    parseFail "/ip4/127.0.0.1/udp/jfodsajfidosajfoidsa"
    parseFail "/ip4/127.0.0.1/udp"
    parseFail "/ip4/127.0.0.1/tcp/jfodsajfidosajfoidsa"
    parseFail "/ip4/127.0.0.1/tcp"
    parseFail "/ip4/127.0.0.1/quic/1234"
    parseFail "/ip4/127.0.0.1/ipfs"
    parseFail "/ip4/127.0.0.1/ipfs/tcp"
    parseFail "/unix"
    parseFail "/ip4/1.2.3.4/tcp/80/unix"
    parseSucceed "/ip4/1.2.3.4" $ Multiaddr [(IP4m $ toIPv4 [1,2,3,4])]
    parseSucceed "/ip4/0.0.0.0" $ Multiaddr [(IP4m $ toIPv4 [0,0,0,0])]
    parseSucceed "/ip4/0.0.0.0//////" $ Multiaddr [(IP4m $ toIPv4 [0,0,0,0])]
    parseSucceed "/ip6/::1" $ Multiaddr [(IP6m $ toIPv6 [0,0,0,0,0,0,0,1])]
    parseSucceed "/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21" $
      Multiaddr
        [
          (IP6m $ toIPv6
           [0x2601, 0x9, 0x4f81, 0x9700, 0x803e, 0xca65, 0x66e8, 0xc21])
        ]
    parseSucceed "/ip6/2601:9:4f81:9700:803e:ca65:66e8:c21/udp/1234/quic" $
      Multiaddr
        [
          (IP6m $ toIPv6
           [0x2601, 0x9, 0x4f81, 0x9700, 0x803e, 0xca65, 0x66e8, 0xc21]),
          (UDPm $ fromJust $ toPort 1234),
          (QUICm)
        ]
    parseSucceed "/onion/timaq4ygg2iegci7:1234" $
      Multiaddr [(toONIONm "timaq4ygg2iegci7" 1234)]
    parseSucceed "/onion/timaq4ygg2iegci7:80/http" $
      Multiaddr
        [
          (toONIONm "timaq4ygg2iegci7" 80),
          (HTTPm)
        ]
    parseSucceed "/udp/1234" $
      Multiaddr [(UDPm $ fromJust $ toPort 1234)]
    parseSucceed "/tcp/1234" $
      Multiaddr [(TCPm $ fromJust $ toPort 1234)]
    parseSucceed "/tcp/1234////" $
      Multiaddr [(TCPm $ fromJust $ toPort 1234)]
    parseSucceed "/udp/65535" $
      Multiaddr [(UDPm $ fromJust $ toPort 65535)]
    parseSucceed "/tcp/65535" $
      Multiaddr [(TCPm $ fromJust $ toPort 65535)]
    parseSucceed "/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC" $
      Multiaddr [(toIPFSm "QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC")]
    parseSucceed "/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC////" $
      Multiaddr [(toIPFSm "QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC")]
    parseSucceed "/udp/1234/sctp/1234" $
      Multiaddr [(UDPm $ fromJust $ toPort 1234), (SCTPm $ fromJust $ toPort 1234)]
    parseSucceed "/udp/1234/udt" $
      Multiaddr [(UDPm $ fromJust $ toPort 1234), UDTm]
    parseSucceed "/udp/1234/utp" $
      Multiaddr [(UDPm $ fromJust $ toPort 1234), UTPm]
    parseSucceed "/tcp/1234/http" $
      Multiaddr [(TCPm $ fromJust $ toPort 1234), HTTPm]
    parseSucceed "/tcp/1234/https" $
      Multiaddr [(TCPm $ fromJust $ toPort 1234), HTTPSm]
    parseSucceed "/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC/tcp/1234" $
      Multiaddr
        [
          (toIPFSm "QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC"),
          (TCPm $ fromJust $ toPort 1234)
        ]
    parseSucceed "/ip4/127.0.0.1/udp/1234" $
      Multiaddr
        [
          (IP4m $ toIPv4 [127,0,0,1]),
          (UDPm $ fromJust $ toPort 1234)
        ]
    parseSucceed "/ip4/127.0.0.1/tcp/1234" $
      Multiaddr
        [
          (IP4m $ toIPv4 [127,0,0,1]),
          (TCPm $ fromJust $ toPort 1234)
        ]
    parseSucceed "/ip4/127.0.0.1/tcp/1234/" $
      Multiaddr
        [
          (IP4m $ toIPv4 [127,0,0,1]),
          (TCPm $ fromJust $ toPort 1234)
        ]
    parseSucceed "/ip4/127.0.0.1/udp/1234/quic" $
      Multiaddr
        [
          (IP4m $ toIPv4 [127,0,0,1]),
          (UDPm $ fromJust $ toPort 1234),
          (QUICm)
        ]
    parseSucceed "/ip4/127.0.0.1/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC" $
      Multiaddr
        [
          (IP4m $ toIPv4 [127,0,0,1]),
          (toIPFSm "QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC")
        ]
    parseSucceed "/ip4/127.0.0.1/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC/tcp/1234" $
      Multiaddr
        [
          (IP4m $ toIPv4 [127,0,0,1]),
          (toIPFSm "QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC"),
          (TCPm $ fromJust $ toPort 1234)
        ]
    parseSucceed "/unix/a/b/c/d/e" $
      Multiaddr [(UNIXm $ UnixPath "/a/b/c/d/e")]
    parseSucceed "/unix/a/b/c/d/e/" $
      Multiaddr [(UNIXm $ UnixPath "/a/b/c/d/e")]
    parseSucceed "/unix/stdio///////" $
      Multiaddr [(UNIXm $ UnixPath "/stdio")]
    parseSucceed "/unix/stdio////a/b/c/d/e/f////" $
      Multiaddr [(UNIXm $ UnixPath "/stdio////a/b/c/d/e/f")]
    parseSucceed "/ip4/1.2.3.4/tcp/80/unix/a/b/c/d/e/f" $
      Multiaddr
        [
          (IP4m $ toIPv4 [1,2,3,4]),
          (TCPm $ fromJust $ toPort 80),
          (UNIXm $ UnixPath "/a/b/c/d/e/f")
        ]
    parseSucceed "/ip4/127.0.0.1/ipfs/QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC/tcp/1234/unix/stdio" $
      Multiaddr
        [
          (IP4m $ toIPv4 [127,0,0,1]),
          (toIPFSm "QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC"),
          (TCPm $ fromJust $ toPort 1234),
          (UNIXm $ UnixPath "/stdio")
        ]
