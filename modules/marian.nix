{ lib, stdenv, fetchFromGitHub, cmake, pcre2, doxygen, graphviz, gperftools
, blas }:

stdenv.mkDerivation rec {
  pname = "marian";
  version = "1.11.0";

  src = fetchFromGitHub {
    rev = version;
    owner = "marian-nmt";
    repo = pname;
    sha256 = "sha256-t5PSFYp/ROhR91m4n/14oM3IWJPs79DYbrI7haYPWKc=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake ];

  cmakeFlags = [
    "-DCOMPILE_CUDA=off"
    "-DCMAKE_BUILD_TYPE=Release"
    # By default it uses BUILD_ARCH=native but doesn't set sse flags
    # which results in a compilation error
    "-DBUILD_ARCH=x86-64"
    "-DCOMPILE_AVX512=off"
  ];

  buildInputs = [ pcre2 doxygen graphviz gperftools blas ];

  patches = [ ./marian.patch ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp marian-vocab $out/bin
    cp marian-decoder $out/bin
    cp marian-conv $out/bin
    cp marian-scorer $out/bin
    cp marian $out/bin
    cp spm_decode $out/bin
    cp spm_encode $out/bin
    cp spm_export_vocab $out/bin
    cp spm_normalize $out/bin
    cp spm_train $out/bin
    runHook postInstall
  '';

  meta = with lib; {
    description =
      "Marian is an efficient Neural Machine Translation framework written in pure C++ with minimal dependencies.";
    homepage = "https://marian-nmt.github.io/";
    license = licenses.mit;
    maintainers = with maintainers; [ kurnevsky ];
  };
}
