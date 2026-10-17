{
  lib,
  rustPlatform,
  fetchFromGitLab,
  pkg-config,
  libevdev,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "hkdm";
  version = "0.3.2";

  src = fetchFromGitLab {
    domain = "gitlab.postmarketos.org";
    owner = "postmarketOS";
    repo = "hkdm";
    tag = finalAttrs.version;
    hash = "sha256-p3QTAjGuXCKsMoLzlMwaJV24XC8FcfFeKY0anNuBzwI=";
  };

  cargoLock.lockFile = ./hkdm-Cargo.lock;

  nativeBuildInputs = [ pkg-config ];

  buildInputs = [ libevdev ];

  meta = with lib; {
    description = "Lighter-weight hotkey daemon";
    homepage = "https://gitlab.postmarketos.org/postmarketOS/hkdm";
    license = licenses.gpl3Only;
    mainProgram = "hkdm";
    platforms = platforms.linux;
  };
})
