{
  description = "abstract-machine-zoo";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/8f3cf34b8d2e2caf4ae5ee1d1fddc1baab4c5964";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, flake-utils, ... }@inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              abstract-machine-zoo = (hfinal.callPackage ./default.nix {});
            };
        };
        abstract-machine-zoo =
          with final.haskell.lib.compose;
          overrideCabal (drv: {
            disallowGhcReference = false;
            enableSeparateDataOutput = false;
          }) (justStaticExecutables final.haskellPackages.abstract-machine-zoo);
      };

      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells.default = pkgs.callPackage ./shell.nix { hspkgs = hspkgs; };
          packages.default = pkgs.abstract-machine-zoo;
        };
    in
    { inherit overlay; } // flake-utils.lib.eachDefaultSystem perSystem;
}
