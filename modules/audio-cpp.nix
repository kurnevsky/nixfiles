{
  inputs,
  lib,
  stdenv,
  config,
  git,
  cmake,

  shaderc,
  vulkan-headers,
  vulkan-loader,
  vulkanSupport ? false,
}:

stdenv.mkDerivation {
  pname = "audio-cpp";
  version = "0";

  src = inputs.audio-cpp;

  nativeBuildInputs = [
    cmake
    git
  ];

  buildInputs = lib.optionals vulkanSupport [
    shaderc
    vulkan-headers
    vulkan-loader
  ];

  postPatch = ''
    echo "install(TARGETS audiocpp_cli audiocpp_server DESTINATION bin)" >> CMakeLists.txt
  '';

  cmakeFlags = [
    (lib.cmakeBool "ENGINE_ENABLE_VULKAN" vulkanSupport)
  ];

  meta = {
    description = "An all-in-one, pure C++ inference engine for audio models, powered by ggml";
    homepage = "https://github.com/0xShug0/audio.cpp";
    license = lib.licenses.asl20;
    mainProgram = "audiocpp_cli";
  };
}
