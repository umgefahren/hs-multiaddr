module Main where

import Multiaddr

parseAddress :: Multiaddr
parseAddress = read "/ip4/10.0.0.1/ip4/10.0.0.1/tcp/429"

-- we need the test cases

main :: IO ()
main = putStrLn $ show parseAddress
