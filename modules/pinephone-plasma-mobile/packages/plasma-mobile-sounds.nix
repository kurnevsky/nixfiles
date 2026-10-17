{
  lib,
  stdenv,
  fetchurl,
  cmake,
  extra-cmake-modules,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "plasma-mobile-sounds";
  version = "0.1";

  src = fetchurl {
    url = "mirror://kde/stable/plasma-mobile-sounds/${finalAttrs.version}/plasma-mobile-sounds-${finalAttrs.version}.tar.xz";
    hash = "sha256-8a7T3dHeIJ4NYN9U6WixQbTIaP8MRwbe24XkzOKfJq8=";
  };

  nativeBuildInputs = [
    cmake
    extra-cmake-modules
  ];

  dontWrapQtApps = true;

  meta = with lib; {
    description = "Sounds for Plasma Mobile devices";
    homepage = "https://invent.kde.org/plasma-mobile/plasma-mobile-sounds";
    license = with licenses; [
      cc-by-40
      cc-by-sa-40
      cc0
    ];
    platforms = platforms.linux;
  };
})
