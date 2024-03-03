{ lib, stdenv, fetchFromGitHub, meson, pkg-config, vulkan-loader, ninja
, writeText, vulkan-headers, vulkan-utility-libraries, jq, libX11, libXrandr
, libxcb, wayland }:

stdenv.mkDerivation rec {
  pname = "vulkan-hdr-layer";
  version = "63d2eec";

  src = (fetchFromGitHub {
    owner = "Zamundaaa";
    repo = "VK_hdr_layer";
    rev = "869199cd2746e7f69cf19955153080842b6dacfc";
    fetchSubmodules = true;
    hash = "sha256-xfVYI+Aajmnf3BTaY2Ysg5fyDO6SwDFGyU0L+F+E3is=";
  }).overrideAttrs (_: {
    GIT_CONFIG_COUNT = 1;
    GIT_CONFIG_KEY_0 = "url.https://github.com/.insteadOf";
    GIT_CONFIG_VALUE_0 = "git@github.com:";
  });

  nativeBuildInputs = [ vulkan-headers meson ninja pkg-config jq ];

  buildInputs = [
    vulkan-headers
    vulkan-loader
    vulkan-utility-libraries
    libX11
    libXrandr
    libxcb
    wayland
  ];

  # Help vulkan-loader find the validation layers
  setupHook = writeText "setup-hook" ''
    addToSearchPath XDG_DATA_DIRS @out@/share
  '';

  meta = with lib; {
    description = "Layers providing Vulkan HDR";
    homepage = "https://github.com/Zamundaaa/VK_hdr_layer";
    platforms = platforms.linux;
    license = licenses.mit;
  };
}
