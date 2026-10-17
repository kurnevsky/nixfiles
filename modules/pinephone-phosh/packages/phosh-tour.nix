{
  lib,
  stdenv,
  fetchurl,
  meson,
  ninja,
  pkg-config,
  desktop-file-utils,
  glib,
  wrapGAppsHook4,
  gtk4,
  libadwaita,
  gmobile,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "phosh-tour";
  version = "0.57.0";

  src = fetchurl {
    url = "https://sources.phosh.mobi/releases/phosh-tour/phosh-tour-${finalAttrs.version}.tar.xz";
    hash = "sha256-2Ihxoq67znGQ7d0GfXetgKodSUUO2mPeozrraA9hbSU=";
  };

  postPatch = ''
    substituteInPlace data/mobi.phosh.PhoshTour-first-login.service.in \
      --replace-fail /usr/bin/phosh-tour $out/bin/phosh-tour
  '';

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    desktop-file-utils
    glib
    wrapGAppsHook4
  ];

  buildInputs = [
    gtk4
    libadwaita
    gmobile
  ];

  meta = with lib; {
    description = "Simple introduction to Phosh";
    homepage = "https://gitlab.gnome.org/World/Phosh/phosh-tour";
    license = licenses.gpl3Plus;
    mainProgram = "phosh-tour";
    platforms = platforms.linux;
  };
})
