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
    rev = "2dc5f28b0c9ceb986f68465bbe4225c50384c110";
    hash = "sha256-GeS/ULpysdY+C63l+8bFBGc9T2YejhdXzx+cOUjq38Q=";
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
