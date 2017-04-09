import Test.Hspec
import Control.Exception (evaluate)
import Multiaddr

parseFail str = it ("should not parse `" ++ str ++ "`") $ do
  evaluate (read str :: Multiaddr) `shouldThrow` anyException

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
