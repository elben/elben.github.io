# https://github.com/Gabriel439/haskell-nix/tree/master/project1
let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          elbenshiracom =
            haskellPackagesNew.callPackage ./elbenshiracom.nix { };

          # Since pencil 0.1.3 is not in the nix channels yet.
          # cabal2nix cabal://pencil-0.1.3 > pencil-0.1.3.nix
          pencil =
            haskellPackagesNew.callPackage ./pencil-0.1.3.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { elbenshiracom = pkgs.haskellPackages.elbenshiracom;
  }

