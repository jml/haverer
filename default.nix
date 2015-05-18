{ mkDerivation, base, basic-prelude, containers, errors, lens, MonadRandom
, mtl, random-shuffle, stdenv, tasty, tasty-hunit, tasty-quickcheck, text
}:
mkDerivation {
  pname = "haverer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = [
    base basic-prelude containers errors lens MonadRandom mtl random-shuffle
    tasty tasty-quickcheck text
  ];
  testDepends = [
    base basic-prelude containers errors mtl random-shuffle tasty tasty-hunit
    tasty-quickcheck text
  ];
  license = stdenv.lib.licenses.asl20;
}
