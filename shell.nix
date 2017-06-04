{nixpkgs ? import <nixpkgs> {}, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "hs-multiaddr";
  buildInputs = [ stack zlib cabal-install ];
  inherit ghc;
}
