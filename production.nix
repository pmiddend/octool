{ mkDerivation, base, bytestring, cabal-install, contravariant
, directory, hindent, hlint, optparse-applicative, process, stdenv
, xeno
}:
mkDerivation {
  pname = "octool";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring contravariant directory optparse-applicative
    process xeno
  ];
  executableToolDepends = [ cabal-install hindent hlint ];
  description = "Helper tool for building Opencast without hassle";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
