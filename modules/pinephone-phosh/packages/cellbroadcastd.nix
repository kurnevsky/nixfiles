{
  lib,
  stdenv,
  fetchurl,
  meson,
  ninja,
  pkg-config,
  gobject-introspection,
  vala,
  glib,
  gmobile,
  mobile-broadband-provider-info,
  modemmanager,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "cellbroadcastd";
  version = "0.0.3";

  src = fetchurl {
    url = "https://sources.phosh.mobi/releases/cellbroadcastd/cellbroadcastd-${finalAttrs.version}.tar.xz";
    hash = "sha256-aG1EzQLiPvqt4NKOo/NNllYthW22d2/Qr028D+OjA5I=";
  };

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    glib
    gobject-introspection
    vala
  ];

  buildInputs = [
    glib
    gmobile
    mobile-broadband-provider-info
    modemmanager
  ];

  mesonFlags = [
    (lib.mesonBool "tests" false)
    (lib.mesonOption "systemd_user_unit_dir" "${placeholder "out"}/lib/systemd/user")
  ];

  meta = with lib; {
    description = "Manages cell broadcast messages received via ModemManager";
    homepage = "https://gitlab.freedesktop.org/devrtz/cellbroadcastd";
    license = licenses.lgpl21Plus;
    platforms = platforms.linux;
  };
})
