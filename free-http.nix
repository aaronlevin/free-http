{ mkDerivation, base, bytestring, free, http-client, http-types
, mtl, stdenv, text, transformers
}:
mkDerivation {
  pname = "free-http";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [
    base bytestring free http-client http-types mtl text transformers
  ];
  homepage = "https://github.com/aaronlevin/free-http";
  description = "Free Monad-based HTTP Client";
  license = stdenv.lib.licenses.mit;
}
