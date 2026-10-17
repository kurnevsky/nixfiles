{
  lib,
  stdenvNoCC,
  fetchurl,
  meson,
  ninja,
  gettext,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "phosh-wallpapers";
  version = "0.55.0";

  src = fetchurl {
    url = "https://sources.phosh.mobi/releases/phosh-wallpapers/phosh-wallpapers-${finalAttrs.version}.tar.xz";
    hash = "sha256-jDAit3h0I2rd68+vWDWFLLLXu+Nhw31m6XbZKgVOYvs=";
  };

  nativeBuildInputs = [
    meson
    ninja
    gettext
  ];

  meta = with lib; {
    description = "Phosh wallpapers, sound theme and other artwork";
    homepage = "https://gitlab.gnome.org/World/Phosh/phosh-wallpapers";
    license = with licenses; [
      gpl3Plus
      cc-by-sa-40
      cc0
    ];
    platforms = platforms.linux;
  };
})
