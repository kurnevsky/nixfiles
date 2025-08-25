{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  fetchpatch,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "PNEink";
  version = "1.3";

  src = fetchFromGitHub {
    owner = "PNDeb";
    repo = "PNEink";
    tag = "v${finalAttrs.version}";
    hash = "sha256-4aoIDJpSM99UxtkQbqu5cClxy1KSCsYoC5cwuqYQDiw=";
  };

  patches = [
    (fetchpatch {
      url = "https://patch-diff.githubusercontent.com/raw/PNDeb/PNEink/pull/2.patch";
      hash = "sha256-EpEBoOyPIeBr1XgGAR4xTw71moOcu3PkQpGYRlyI7go=";
    })
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/themes/PNEink/

    cp -r gnome-shell $out/share/themes/PNEink/
    cp -r common $out/share/themes/PNEink/
    cp -r gtk-3.0 $out/share/themes/PNEink/

    runHook postInstall
  '';

  meta = with lib; {
    description = "Gnome Theme designed for use with the Pine64 PineNote";
    homepage = "https://github.com/PNDeb/PNEink";
    platforms = platforms.linux;
  };
})
