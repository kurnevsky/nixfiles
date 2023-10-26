{ pkgs, pkgsi686Linux, callPackage, fetchFromGitHub, stdenv_32bit, pkgsCross, ... }:

let
  sources =
    (import "${pkgs.path}/pkgs/applications/emulators/wine/sources.nix" {
      inherit pkgs;
    }).unstable;
in callPackage "${pkgs.path}/pkgs/applications/emulators/wine/base.nix" rec {
  pname = "wine-ge";
  version = "Proton8-21";
  src = fetchFromGitHub {
    owner = "GloriousEggroll";
    repo = "proton-wine";
    rev = version;
    hash = "sha256-yB0rFZTlQSzKu1GCyW4yMJBkzBvErZkfB3UqiY40e00=";
  };
  moltenvk = pkgs.moltenvk;
  patches = [ ];
  stdenv = stdenv_32bit;
  pkgArches = [ pkgs pkgsi686Linux ];
  geckos = with sources; [ gecko32 gecko64 ];
  mingwGccs = with pkgsCross; [
    mingw32.buildPackages.gcc
    mingwW64.buildPackages.gcc
  ];
  monos = with sources; [ mono ];
  buildScript = "${pkgs.path}/pkgs/applications/emulators/wine/builder-wow.sh";
  configureFlags = [ "--disable-tests" ];
  platforms = [ "x86_64-linux" ];
  mainProgram = "wine64";
  supportFlags = {
    gettextSupport = true;
    fontconfigSupport = true;
    alsaSupport = true;
    gtkSupport = true;
    openglSupport = true;
    tlsSupport = true;
    gstreamerSupport = true;
    cupsSupport = true;
    dbusSupport = true;
    openclSupport = true;
    cairoSupport = true;
    odbcSupport = true;
    netapiSupport = true;
    cursesSupport = true;
    vaSupport = true;
    pcapSupport = true;
    v4lSupport = true;
    saneSupport = true;
    gphoto2Support = true;
    krb5Support = true;
    pulseaudioSupport = true;
    udevSupport = true;
    xineramaSupport = true;
    vulkanSupport = true;
    sdlSupport = true;
    usbSupport = true;
    mingwSupport = true;
    x11Support = true;
    waylandSupport = false;
    embedInstallers = true;
  };
}
