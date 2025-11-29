{
  inputs,
  lib,
  stdenv,
  config,
  git,
  cmake,
  rocmPackages,
  useRocm ? config.rocmSupport,
  gpuTargets ? builtins.concatStringsSep ";" rocmPackages.clr.gpuTargets,
}:

stdenv.mkDerivation {
  pname = "stable-diffusion-cpp";
  version = "0";

  src = inputs.stable-diffusion-cpp;

  nativeBuildInputs = [
    cmake
    git
  ];

  buildInputs = lib.optionals useRocm (
    with rocmPackages;
    [
      clr
      hipblas
      rocblas
    ]
  );

  cmakeFlags =
    [
      (lib.cmakeBool "GGML_HIP" useRocm)
      (lib.cmakeBool "SD_HIPBLAS" useRocm)
    ]
    ++ lib.optionals useRocm [
      (lib.cmakeFeature "CMAKE_HIP_COMPILER" "${rocmPackages.clr.hipClangPath}/clang++")
      (lib.cmakeFeature "CMAKE_HIP_ARCHITECTURES" gpuTargets)
      (lib.cmakeFeature "AMDGPU_TARGETS" gpuTargets)
    ];

  env = lib.optionalAttrs useRocm {
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
