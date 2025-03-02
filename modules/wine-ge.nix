{
  pkgs,
  pkgsi686Linux,
  callPackage,
  fetchFromGitHub,
  stdenv_32bit,
  pkgsCross,
  ...
}:

let
  sources =
    (import "${pkgs.path}/pkgs/applications/emulators/wine/sources.nix" {
      inherit pkgs;
    }).unstable;
in
(callPackage "${pkgs.path}/pkgs/applications/emulators/wine/base.nix" rec {
  pname = "wine-ge";
  version = "Proton8-26";
  src = fetchFromGitHub {
    owner = "GloriousEggroll";
    repo = "proton-wine";
    rev = "39021e609a24b6aeffdf9c4695a286d71e7dffbc";
    hash = "sha256-yJnJ7D/QHvZ+ieR8nAAnkFV8pzQkDaYDs0Xvo9YsDQA=";
  };
  wineRelease = "ge";
  inherit (pkgs) moltenvk;
  patches = [ ];
  stdenv = stdenv_32bit;
  pkgArches = [
    pkgs
    pkgsi686Linux
  ];
  geckos = with sources; [
    gecko32
    gecko64
  ];
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
    waylandSupport = true;
    embedInstallers = true;
  };
}).overrideAttrs
  (_old: {
    NIX_CFLAGS_COMPILE = [
      "-Wno-error=incompatible-pointer-types"
    ];
  })
