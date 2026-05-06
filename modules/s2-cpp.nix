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
  pname = "s2-cpp";
  version = "0";

  src = fetchFromGitHub {
    owner = "rodrigomatta";
    repo = "s2.cpp";
    rev = "e48ce8e02d8335bd9a0ba94679f605724b31d123";
    hash = "sha256-aocv7MTQV4I4xsapQhyY9nREYM/nNkt2YsuojkQQ2Lc=";
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
    (lib.cmakeBool "S2_VULKAN" vulkanSupport)
  ];

  meta = {
    description = "Implementation of Fish Audio S2 Pro model inference in native ggml";
    homepage = "https://github.com/rodrigomatta/s2.cpp";
    mainProgram = "s2";
  };
}
