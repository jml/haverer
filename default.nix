{ mkDerivation, base, containers, lens, MonadRandom, pretty-show
, random-shuffle, stdenv, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "haverer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base containers lens MonadRandom pretty-show random-shuffle tasty
    tasty-quickcheck
  ];
  testDepends = [
    base containers random-shuffle tasty tasty-hunit tasty-quickcheck
  ];
  license = stdenv.lib.licenses.asl20;
}
