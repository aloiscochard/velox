with import <nixpkgs> {};
{
  sdlEnv = stdenv.mkDerivation {
    name = "velox";
    buildInputs = [
      rustup pkgconfig
    ];
  };
}
