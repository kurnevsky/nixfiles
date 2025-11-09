{
  stdenv,
  fetchFromGitHub,
  pkg-config,
  meson,
  ninja,
  cmake,
  vapoursynth,
  vulkan-loader,
  ...
}:

stdenv.mkDerivation rec {
  pname = "VapourSynth-RIFE-ncnn-Vulkan";
  version = "r9_mod_v33";

  src = fetchFromGitHub {
    owner = "styler00dollar";
    repo = pname;
    tag = version;
    fetchSubmodules = true;
    hash = "sha256-j1ETwr8DF+EOThDOzF5OMHWoFmRD4gNW/tSj6f/n1Vk=";
  };

  nativeBuildInputs = [
    pkg-config
    meson
    ninja
    cmake
  ];

  buildInputs = [
    vapoursynth
    vulkan-loader
  ];

  postPatch = ''
    substituteInPlace meson.build \
      --replace-fail "'b_lto=true'," "" \
      --replace-fail "vapoursynth_dep.get_variable(pkgconfig: 'libdir')" "get_option('libdir')"
  '';
}
