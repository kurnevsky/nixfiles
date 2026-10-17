{
  lib,
  stdenv,
  config,
  git,
  cmake,
  fetchFromGitHub,

  shaderc,
  vulkan-headers,
  vulkan-loader,
  vulkanSupport ? false,
}:

stdenv.mkDerivation {
  pname = "CrispASR";
  version = "0";

  src = fetchFromGitHub {
    owner = "CrispStrobe";
    repo = "CrispASR";
    rev = "dd14099c35d297364092576466b12e622d399b41";
    fetchSubmodules = true;
    hash = "sha256-E2U0P5HxjpeZ4GFhFmhYaI+tf16Tzn2YFFM2Y2Kvv+E=";
  };

  nativeBuildInputs = [
    cmake
    git
  ];

  buildInputs = lib.optionals vulkanSupport [
    shaderc
    vulkan-headers
    vulkan-loader
  ];

  cmakeFlags = [
    (lib.cmakeBool "CRISPASR_BUILD_TESTS" false)
    (lib.cmakeBool "GGML_VULKAN" vulkanSupport)
  ];

  meta = {
    description = "C++ ggml runtime hub for multilingual ASR models";
    homepage = "https://github.com/CrispStrobe/CrispASR";
    mainProgram = "crispasr";
  };
}
