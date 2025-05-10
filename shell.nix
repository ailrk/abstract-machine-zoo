{ pkgs, hspkgs }:
hspkgs.shellFor {
  packages = p: [ p.abstract-machine-zoo ];
  buildInputs = [
    hspkgs.cabal-install
    hspkgs.haskell-language-server
    hspkgs.cabal2nix
    pkgs.ghciwatch
    pkgs.bashInteractive
  ];
}
