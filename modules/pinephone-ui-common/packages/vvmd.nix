{
  lib,
  stdenv,
  fetchFromGitLab,
  meson,
  ninja,
  pkg-config,
  c-ares,
  curl,
  glib,
  libphonenumber,
  mobile-broadband-provider-info,
  modemmanager,
  protobuf,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "vvmd";
  version = "1.1";

  src = fetchFromGitLab {
    owner = "kop316";
    repo = "vvmd";
    tag = finalAttrs.version;
    hash = "sha256-8+mHtkBnbVM/hqT7ElAM7coLRFMW+Sy40PEROra+MA4=";
  };

  patches = [
    # Upstream commit 066f88db, not released yet.
    ./vvmd-systemd-unit.patch
  ];

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
  ];

  buildInputs = [
    c-ares
    curl
    glib
    libphonenumber
    mobile-broadband-provider-info
    modemmanager
    protobuf
  ];

  meta = with lib; {
    description = "Lower level daemon that retrieves Visual Voicemail";
    homepage = "https://gitlab.com/kop316/vvmd";
    license = licenses.gpl2Only;
    mainProgram = "vvmd";
    platforms = platforms.linux;
  };
})
