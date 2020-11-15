with import <nixpkgs> { };

stdenv.mkDerivation {
  name = "tic80";
  src = ./.;

  buildInputs = [cmake pkgconfig ecl doxygen gtk3 xorg.libXext pcre];
  configurePhase = ''
    cd build
    cmake ..
  '';
  buildPhase = ''
    make -j16
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp bin/tic80 $out/bin/tic80
  '';
}
