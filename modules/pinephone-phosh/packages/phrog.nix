# From https://github.com/NixOS/nixpkgs/pull/558022
{
  lib,
  rustPlatform,
  fetchFromGitHub,
  atk,
  cairo,
  gdk-pixbuf,
  glib,
  gnome-settings-daemon,
  gnome-shell,
  gsettings-desktop-schemas,
  gtk3,
  libhandy,
  mutter,
  pango,
  phoc,
  phosh,
  pkg-config,
  wayland,
  wrapGAppsHook3,
  writableTmpDirAsHomeHook,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "phrog";
  version = "0.53.0";

  src = fetchFromGitHub {
    owner = "samcday";
    repo = "phrog";
    tag = finalAttrs.version;
    hash = "sha256-ojr5k6eYONMNDk/DwWU6RxCDv9TrnUskFqL+2X0DIT0=";
  };

  cargoHash = "sha256-0iUemQZzXLu6VfTKj+ov7DZH3u/6aiQziTKok3E0SXg=";

  strictDeps = true;

  nativeBuildInputs = [
    glib
    pkg-config
    wrapGAppsHook3
    writableTmpDirAsHomeHook
  ];

  buildInputs = [
    atk
    cairo
    gdk-pixbuf
    glib
    gnome-settings-daemon
    gsettings-desktop-schemas
    gtk3
    libhandy
    mutter
    pango
    phoc
    # libphosh-0.45.pc, from phosh's -Dbindings-lib=true.
    phosh
    wayland
  ];

  # The suite needs a live phoc, a session bus and accountsservice.
  doCheck = false;

  # org.gnome.shell.keybindings; gnome-shell is schemas-only here
  preFixup = ''
    gappsWrapperArgs+=(
      --prefix XDG_DATA_DIRS : "${glib.getSchemaDataDirPath gnome-shell}"
    )
  '';

  postInstall = ''
    install -Dm644 -t "$out/share/glib-2.0/schemas" \
      data/mobi.phosh.phrog.gschema.xml \
      data/00_mobi.phosh.Phrog.gschema.override
    glib-compile-schemas "$out/share/glib-2.0/schemas"
  '';

  meta = with lib; {
    description = "Mobile-friendly greeter for greetd, built on Phosh";
    homepage = "https://github.com/samcday/phrog";
    license = licenses.gpl3Only;
    mainProgram = "phrog";
    platforms = platforms.linux;
  };
})
