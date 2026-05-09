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
  pname = "qwen3-tts.cpp";
  version = "0";

  src = fetchFromGitHub {
    owner = "Danmoreng";
    repo = "qwen3-tts.cpp";
    rev = "a3606e9d3c869b9ca7406ed5401bd65a8bb6d29b";
    hash = "sha256-el8bWuPWq1ixMTzQXkCyGiFmwlnMWv14e5jyhAh1MdY=";
    fetchSubmodules = true;
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
    (lib.cmakeBool "GGML_VULKAN" vulkanSupport)
  ];

  meta = {
    description = "C++ inference for Qwen3-TTS using the GGML tensor library";
    homepage = "https://github.com/Danmoreng/qwen3-tts.cpp";
    mainProgram = "qwen3-tts-cli";
  };
}
