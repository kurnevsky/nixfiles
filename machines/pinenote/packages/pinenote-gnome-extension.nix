{
  lib,
  stdenv,
  gnome-shell,
  fetchFromGitHub,
  glib,
}:

stdenv.mkDerivation {
  pname = "pinenote-gnome-extension";
  version = "1.8";

  src = fetchFromGitHub {
    owner = "PNDeb";
    repo = "pinenote-gnome-extension";
    rev = "f755cbbce88b46790b96652e2a1e933b230c98c2";
    hash = "sha256-tlK1/uwEFlrZ0yu0YbvCC3iAXCFRFOcKb/CoVa3+8vQ=";
  };

  passthru = {
    extensionUuid = "pnhelper@m-weigand.github.com";
    extensionPortalSlug = "pinenote-gnome-extension";
  };

  nativeBuildInputs = [ glib ];

  buildPhase = ''
    runHook preBuild
    glib-compile-schemas --strict --targetdir="pnhelper@m-weigand.github.com/schemas/" "pnhelper@m-weigand.github.com/schemas"
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/gnome-shell/extensions
    cp -r "pnhelper@m-weigand.github.com" $out/share/gnome-shell/extensions
    runHook postInstall
  '';

  meta = with lib; {
    description = "Gnome extension to interface with some of the rockchip_ebc kernel driver options";
    license = licenses.gpl2Plus;
    homepage = "https://github.com/PNDeb/pinenote-gnome-extension";
  };
}
