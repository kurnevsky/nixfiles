{
  lib,
  stdenvNoCC,
  fetchFromGitLab,
  makeWrapper,
  buffybox,
  coreutils,
  kbd,
  kmod,
  procps,
  psmisc,
  terminus_font,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "ttyescape";
  version = "1.0.2";

  src = fetchFromGitLab {
    domain = "gitlab.postmarketos.org";
    owner = "postmarketOS";
    repo = "ttyescape";
    tag = finalAttrs.version;
    hash = "sha256-5FQR5OBKal+yu/BZVTt7+p6yg0R+5LaYMT37T1NRpH8=";
  };

  nativeBuildInputs = [ makeWrapper ];

  postPatch = ''
    substituteInPlace togglevt.sh \
      --replace-fail /usr/share/consolefonts/ter-128n.psf.gz ${terminus_font}/share/consolefonts/ter-128n.psf.gz
    substituteInPlace ttyescape-hkdm.toml \
      --replace-fail /usr/bin/togglevt.sh $out/bin/togglevt.sh
  '';

  installPhase = ''
    runHook preInstall

    install -Dm755 togglevt.sh $out/bin/togglevt.sh
    install -Dm644 ttyescape-hkdm.toml $out/etc/hkdm/config.d/ttyescape.toml

    wrapProgram $out/bin/togglevt.sh \
      --prefix PATH : ${
        lib.makeBinPath [
          buffybox
          coreutils
          kbd
          kmod
          procps
          psmisc
        ]
      }

    runHook postInstall
  '';

  meta = with lib; {
    description = "Daemon to allow users to escape to a tty";
    homepage = "https://gitlab.postmarketos.org/postmarketOS/ttyescape";
    license = licenses.gpl2Plus;
    platforms = platforms.linux;
  };
})
