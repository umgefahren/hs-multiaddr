module Main where

import qualified Data.Multihash.Digest as MHD
import Multiaddr

parseAddress :: Multiaddr
parseAddress = read "/ip4/10.0.0.1/ip4/10.0.0.1/tcp/429"

parseHash :: MHD.MultihashDigest
parseHash = read "QmcgpsyWgH8Y8ajJz1Cu72KnS5uo2Aa2LpzU7kinSupNKC"

-- we need the test cases

main :: IO ()
main = putStrLn $ show parseHash
