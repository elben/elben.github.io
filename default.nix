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

          # Since pencil 0.1.3 specifies hsass >= 0.8, and the
          # 18.09 nix channel only has hsass 0.7.
          # cabal2nix cabal://hsass-0.8.0 > hsass-0.8.0.nix
          hsass =
            haskellPackagesNew.callPackage ./hsass-0.8.0.nix { };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { elbenshiracom = pkgs.haskellPackages.elbenshiracom;
  }

