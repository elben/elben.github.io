{ mkDerivation, base, pencil, stdenv, text, unordered-containers }:
mkDerivation {
  pname = "elbenshiracom";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base pencil text unordered-containers
  ];
  homepage = "https://github.com/elben/elben.github.io";
  description = "Personal website: elbenshira.com";
  license = stdenv.lib.licenses.bsd3;
}