{
  lib,
  stdenv,
  config,
  cmake,
  fetchFromGitHub,
  blas,

  rocmPackages,
  rocmSupport ? config.rocmSupport,
  gpuTargets ? builtins.concatStringsSep ";" rocmPackages.clr.gpuTargets,

  shaderc,
  vulkan-headers,
  vulkan-loader,
  vulkanSupport ? false,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "transcribe-cpp";
  version = "0.3.0";

  src = fetchFromGitHub {
    owner = "handy-computer";
    repo = "transcribe.cpp";
    tag = "v${finalAttrs.version}";
    hash = "sha256-kxHUPIc1q9m+RzgN5+VecLiM36ANwDO+1CZs8q/1ZmU=";
  };

  nativeBuildInputs = [ cmake ];

  buildInputs =
    [ blas ]
    ++ lib.optionals rocmSupport (
      with rocmPackages;
      [
        clr
        hipblas
        rocblas
      ]
    )
    ++ lib.optionals vulkanSupport [
      shaderc
      vulkan-headers
      vulkan-loader
    ];

  cmakeFlags =
    [
      (lib.cmakeBool "TRANSCRIBE_BUILD_TESTS" false)
      (lib.cmakeBool "TRANSCRIBE_BUILD_TOOLS" true)
      (lib.cmakeBool "TRANSCRIBE_BUILD_SHARED" true)
      (lib.cmakeBool "TRANSCRIBE_HIP" rocmSupport)
      (lib.cmakeBool "TRANSCRIBE_VULKAN" vulkanSupport)
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

  postPatch = ''
    echo "install(TARGETS transcribe-cli DESTINATION bin)" >> examples/cli/CMakeLists.txt
    echo "install(TARGETS transcribe-quantize DESTINATION bin)" >> tools/transcribe-quantize/CMakeLists.txt
    echo "install(TARGETS transcribe-bench DESTINATION bin)" >> tools/transcribe-bench/CMakeLists.txt
  '';

  meta = {
    description = "C/C++ speech-to-text inference library built on ggml";
    homepage = "https://github.com/handy-computer/transcribe.cpp";
    license = lib.licenses.mit;
    mainProgram = "transcribe-cli";
  };
})
