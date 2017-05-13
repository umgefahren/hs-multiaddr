{nixpkgs ? import <nixpkgs> {}, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "haskell-multiaddr";
  buildInputs = [ zlib cabal-install ];
  inherit ghc;
}
