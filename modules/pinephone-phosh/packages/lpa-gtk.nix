{
  lib,
  stdenv,
  fetchFromGitea,
  meson,
  ninja,
  pkg-config,
  blueprint-compiler,
  desktop-file-utils,
  gobject-introspection,
  wrapGAppsHook4,
  gtk4,
  libadwaita,
  lpac,
  python3,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "lpa-gtk";
  version = "0.4";

  src = fetchFromGitea {
    domain = "codeberg.org";
    owner = "lucaweiss";
    repo = "lpa-gtk";
    tag = finalAttrs.version;
    hash = "sha256-1siewcpTuWR29H0FiSkMk4TEVtvIOAn7ACt2XVLn4dA=";
  };

  nativeBuildInputs = [
    meson
    ninja
    pkg-config
    blueprint-compiler
    desktop-file-utils
    gobject-introspection
    wrapGAppsHook4
    python3
    python3.pkgs.wrapPython
  ];

  buildInputs = [
    gtk4
    libadwaita
    python3
  ];

  pythonPath = [ python3.pkgs.pygobject3 ];

  dontWrapGApps = true;

  postFixup = ''
    makeWrapperArgs+=(
      "''${gappsWrapperArgs[@]}"
      --prefix PATH : ${lib.makeBinPath [ lpac ]}
    )
    wrapPythonPrograms
  '';

  meta = with lib; {
    description = "Download and manage eSIM profiles";
    homepage = "https://codeberg.org/lucaweiss/lpa-gtk";
    license = licenses.gpl3Only;
    mainProgram = "lpa-gtk";
    platforms = platforms.linux;
  };
})
