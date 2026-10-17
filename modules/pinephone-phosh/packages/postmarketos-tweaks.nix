{
  lib,
  stdenvNoCC,
  fetchFromGitLab,
  meson,
  ninja,
  gettext,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "postmarketos-tweaks-setting-definitions";
  version = "1.1.0";

  src = fetchFromGitLab {
    domain = "gitlab.postmarketos.org";
    owner = "postmarketOS";
    repo = "postmarketos-tweaks-setting-definitions";
    tag = finalAttrs.version;
    hash = "sha256-F/iqLByeW0vbpLDCYQeTodgI5zIKK5xjxKnzlIf9mUU=";
  };

  postPatch = ''
    substituteInPlace meson.build \
      --replace-fail "'/usr/share/postmarketos-tweaks'" "get_option('datadir') / 'postmarketos-tweaks'"
  '';

  nativeBuildInputs = [
    meson
    ninja
    gettext
  ];

  # sxmo-x11.yml is for another UI; pinephone.yml needs pmOS's update-sysfs.
  postInstall = ''
    rm $out/share/postmarketos-tweaks/{pinephone,sxmo-x11}.yml
  '';

  meta = with lib; {
    description = "Setting definitions for programs that implement the conf-tweaks format";
    homepage = "https://gitlab.postmarketos.org/postmarketOS/postmarketos-tweaks-setting-definitions";
    license = licenses.gpl3Plus;
    platforms = platforms.linux;
  };
})
