Matrix Multi Address
=========================

Here is an experiment to combine the haskell implementation of Multiaddress with the Haskell implementation of Multihash.

Along the way, we must learn how stack works.

-- Multiaddr is a record wrapping a list of MultiaddrParts
-- a Multiaddr here is a typed Multiaddr within Haskell
-- in the outside world, there are 2 kinds of Multiaddr
-- a textual human readable version
-- a binary encoded version
-- this library exposes the typed Multiaddr
-- while allowing Multiaddr to be created from
-- human readable text or binary encoded string

There is any number of slashes.
