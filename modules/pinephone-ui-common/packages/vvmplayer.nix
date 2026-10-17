{
  lib,
  stdenv,
  fetchFromGitLab,
  meson,
  ninja,
  pkg-config,
  desktop-file-utils,
  wrapGAppsHook4,
  callaudiod,
  evolution-data-server,
  glib,
  gst_all_1,
  gtk4,
  libadwaita,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "vvmplayer";
  version = "2.6";

  src = fetchFromGitLab {
    owner = "kop316";
    repo = "vvmplayer";
    tag = finalAttrs.version;
    hash = "sha256-eSEXemihUDd3I3uxtIyfZJ5S5j9dXdv7w/D65TmNs6g=";
  };

  patches = [
    # From Alpine.
    ./vvmplayer-eds-3.60.patch
  ];

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    desktop-file-utils
    glib
    wrapGAppsHook4
  ];

  buildInputs = [
    callaudiod
    evolution-data-server
    glib
    gst_all_1.gstreamer
    gst_all_1.gst-plugins-base
    gst_all_1.gst-plugins-good
    gst_all_1.gst-plugins-ugly
    gst_all_1.gst-libav
    gtk4
    libadwaita
  ];

  meta = with lib; {
    description = "Front end GUI to display, play, and delete visual voicemails";
    homepage = "https://gitlab.com/kop316/vvmplayer";
    license = licenses.gpl3Only;
    mainProgram = "vvmplayer";
    platforms = platforms.linux;
  };
})
