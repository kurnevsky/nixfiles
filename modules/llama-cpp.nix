{ lib, stdenv, fetchFromGitHub, cmake, makeWrapper, python3, ocl-icd
, opencl-headers }:

let
  clblast = stdenv.mkDerivation rec {
    pname = "CLBlast";
    version = "1.5.3";

    src = fetchFromGitHub {
      owner = "CNugteren";
      repo = pname;
      rev = version;
      hash = "sha256-1FNt+lVt8AMkkEtkP61ND2ZmsMJ3ZuU2m2yIUt51mSg=";
    };

    postPatch = ''
      substituteInPlace clblast.pc.in \
        --replace '$'{exec_prefix}/@CMAKE_INSTALL_LIBDIR@ @CMAKE_INSTALL_FULL_LIBDIR@ \
        --replace '$'{prefix}/@CMAKE_INSTALL_INCLUDEDIR@ @CMAKE_INSTALL_FULL_INCLUDEDIR@
    '';

    nativeBuildInputs = [ cmake ];
    buildInputs = [ ocl-icd opencl-headers ];
  };
in stdenv.mkDerivation rec {
  pname = "llama-cpp";
  version = "unstable-2023-08-25";

  src = fetchFromGitHub {
    owner = "ggerganov";
    repo = "llama.cpp";
    rev = "232caf3c1581a6cb023571780ff41dc2d66d1ca0";
    hash = "sha256-EEGaNqGsjY+6U2i5eEc9BC8XmntRFqgd5Be9/4N4PIs=";
  };

  nativeBuildInputs = [ cmake makeWrapper ];

  buildInputs = [ clblast ];

  pythonEnv = python3.withPackages
    (ps: with ps; [ numpy torch numba tqdm sentencepiece ]);

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    mkdir -p $out/libexec

    cp ./bin/main $out/bin/llama-cpp
    cp ./bin/perplexity $out/bin/llama-cpp-perplexity
    cp ./bin/embedding $out/bin/llama-cpp-embedding
    cp ./bin/quantize $out/bin/llama-cpp-quantize

    cp ../*.py $out/libexec/

    for f in $out/libexec/*.py; do
      makeWrapper ${pythonEnv}/bin/python "$out/bin/$(basename $f .py)" \
        --add-flags "$f"
    done

    runHook postInstall
  '';

  cmakeFlags = [ "-DLLAMA_CLBLAST=ON" ];

  meta = with lib; {
    description = "Port of Facebook's LLaMA model in C/C++";
    homepage = "https://github.com/ggerganov/llama.cpp";
    license = licenses.mit;
    mainProgram = "llama-cpp";
    maintainers = with maintainers; [ kurnevsky ];
  };
}
