let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskellPackages.callPackage ./elbenshiracom.nix { }
