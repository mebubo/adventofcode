{ mkDerivation, array, base, containers, grid, hpack, megaparsec
, mtl, operational, parsec, pointedlist, split, stdenv, vector
}:
mkDerivation {
  pname = "adventofcode2017";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base containers grid megaparsec mtl operational parsec
    pointedlist split vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    array base containers grid megaparsec mtl operational parsec
    pointedlist split vector
  ];
  preConfigure = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
