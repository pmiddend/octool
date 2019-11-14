{ mkDerivation, base, cabal-install, hindent, hlint, stdenv }:
mkDerivation {
  pname = "octool";
  version = "1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  executableToolDepends = [ cabal-install hindent hlint ];
  description = "Helper tool for building Opencast without hassle";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
