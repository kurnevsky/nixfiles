{
  inputs,
  lib,
  stdenv,
  config,
  git,
  cmake,

  rocmPackages,
  rocmSupport ? config.rocmSupport,
  gpuTargets ? builtins.concatStringsSep ";" rocmPackages.clr.gpuTargets,

  shaderc,
  vulkan-headers,
  vulkan-loader,
  vulkanSupport ? false,
}:

stdenv.mkDerivation {
  pname = "stable-diffusion-cpp";
  version = "0";

  src = inputs.stable-diffusion-cpp;

  nativeBuildInputs = [
    cmake
    git
  ];

  buildInputs = lib.optionals rocmSupport (
    with rocmPackages;
    [
      clr
      hipblas
      rocblas
    ]
  ) ++ lib.optionals vulkanSupport [
    shaderc
    vulkan-headers
    vulkan-loader
  ];

  cmakeFlags =
    [
      (lib.cmakeBool "GGML_HIP" rocmSupport)
      (lib.cmakeBool "SD_HIPBLAS" rocmSupport)
      (lib.cmakeBool "SD_VULKAN" vulkanSupport)
    ]
    ++ lib.optionals rocmSupport [
      (lib.cmakeFeature "CMAKE_HIP_COMPILER" "${rocmPackages.clr.hipClangPath}/clang++")
      (lib.cmakeFeature "CMAKE_HIP_ARCHITECTURES" gpuTargets)
      (lib.cmakeFeature "AMDGPU_TARGETS" gpuTargets)
    ];

  env = lib.optionalAttrs rocmSupport {
    ROCM_PATH = "${rocmPackages.clr}";
    HIP_DEVICE_LIB_PATH = "${rocmPackages.rocm-device-libs}/amdgcn/bitcode";
  };

  meta = {
    description = "Stable Diffusion in pure C/C++";
    homepage = "https://github.com/leejet/stable-diffusion.cpp";
    license = lib.licenses.mit;
    mainProgram = "sd";
  };
}
