{ stdenv, xorg }:

stdenv.mkDerivation {
  name = "xcb-client-id";
  src = ./xcb-client-id;
  buildInputs = [ xorg.libxcb ];
  buildPhase = "gcc -lxcb -lxcb-res -o xcb-client-id main.c";
  installPhase = "mkdir -p $out/bin && cp xcb-client-id $out/bin";
}
