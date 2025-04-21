{
  lib,
  stdenv,
  config,
  fetchFromGitHub,
  cmake,
  rocmPackages,
  useRocm ? config.rocmSupport,
  gpuTargets ? builtins.concatStringsSep ";" rocmPackages.clr.gpuTargets,
}:

stdenv.mkDerivation {
  pname = "stable-diffusion-cpp";
  version = "0";

  src = fetchFromGitHub {
    owner = "leejet";
    repo = "stable-diffusion.cpp";
    rev = "dcf91f9e0f2cbf9da472ee2a556751ed4bab2d2a";
    hash = "sha256-NHIjLZNfx9G6olp0VWBthuf7jIQC/qVRw6q9A6H866E=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake ];

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

  env = lib.optionals useRocm {
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
