{ mkDerivation, base, stdenv, text, unordered-containers }:
mkDerivation {
  pname = "elbenshiracom";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base text unordered-containers JuicyPixels
  ];
  homepage = "https://github.com/elben/elben.github.io";
  description = "Personal website: elbenshira.com";
  license = stdenv.lib.licenses.bsd3;
}
