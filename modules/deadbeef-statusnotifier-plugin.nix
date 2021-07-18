{ lib, stdenv, fetchurl, pkg-config, deadbeef, gtk3, perl, libdbusmenu-glib }:

stdenv.mkDerivation rec {
  pname = "deadbeef-statusnotifier-plugin";
  version = "1.6";

  src = fetchurl {
    url =
      "https://github.com/vovochka404/deadbeef-statusnotifier-plugin/archive/refs/tags/v${version}.tar.gz";
    sha256 = "sha256-jl2NUD2hv1ee8fz8L7xNxTfdhSLeUrmqL/yuCIXikXA=";
  };

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ deadbeef gtk3 libdbusmenu-glib ];

  buildFlags = [ "gtk3" ];

  postPatch = [''
    substituteInPlace tools/glib-mkenums \
      --replace /usr/bin/perl "${perl}/bin/perl"
  ''];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/lib/deadbeef
    cp build/sni_gtk3.so $out/lib/deadbeef
    runHook postInstall
  '';

  meta = with lib; {
    description = "DeaDBeeF StatusNotifier Plugin";
    homepage = "https://github.com/vovochka404/deadbeef-statusnotifier-plugin";
    license = licenses.gpl3;
    platforms = platforms.linux;
  };
}
