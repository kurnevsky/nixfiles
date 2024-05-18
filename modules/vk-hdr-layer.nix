{ lib, stdenv, fetchFromGitHub, meson, pkg-config, vulkan-loader, ninja
, writeText, vulkan-headers, vulkan-utility-libraries, jq, libX11, libXrandr
, libxcb, wayland }:

stdenv.mkDerivation rec {
  pname = "vulkan-hdr-layer";
  version = "63d2eec";

  src = (fetchFromGitHub {
    owner = "Drakulix";
    repo = "VK_hdr_layer";
    rev = "e47dc6da924cd361b0082f5c27fe5e923377bb54";
    fetchSubmodules = true;
    hash = "sha256-wuZdUWMKEM/UCeuZSiNyup2vzo6+KIH9Rpaoc4FARJE=";
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
