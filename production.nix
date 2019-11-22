{ mkDerivation, base, bytestring, cabal-install, directory
, filepath, hindent, hlint, optparse-applicative, polysemy, process
, stdenv, xeno
}:
mkDerivation {
  pname = "octool";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring directory filepath optparse-applicative polysemy
    process xeno
  ];
  executableToolDepends = [ cabal-install hindent hlint ];
  description = "Helper tool for building Opencast without hassle";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
