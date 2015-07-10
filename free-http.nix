{ mkDerivation, base, bytestring, free, http-client, http-types
, mtl, QuickCheck, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "free-http";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base bytestring free http-client http-types mtl QuickCheck text
    time transformers
  ];
  homepage = "https://github.com/aaronlevin/free-http";
  description = "Free Monad-based HTTP Client";
  license = stdenv.lib.licenses.mit;
}
