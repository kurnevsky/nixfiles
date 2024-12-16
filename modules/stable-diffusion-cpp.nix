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
    rev = "4570715727f35e5a07a76796d823824c8f42206c";
    hash = "sha256-1w7OokrQflasvauDEADLDJf2530m5a7WP+X1KgwxCks=";
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
      (lib.cmakeFeature "CMAKE_HIP_COMPILER" "${rocmPackages.llvm.clang}/bin/clang")
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
