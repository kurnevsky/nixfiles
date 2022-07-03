{ lib, stdenv, fetchFromGitHub, cmake, pcre2, doxygen, graphviz, gperftools
, blas }:

stdenv.mkDerivation rec {
  pname = "bergamot-translator";
  version = "0.4.5";

  src = fetchFromGitHub {
    rev = "v${version}";
    owner = "browsermt";
    repo = "bergamot-translator";
    sha256 = "sha256-O1tia5OntcCpi4wPqQYu7gmWF+L5oMVEkGB7OcpjvK4=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [ cmake ];

  cmakeFlags = [
    "-DUSE_WASM_COMPATIBLE_SOURCE=off"
    "-DCMAKE_BUILD_TYPE=Release"
    "-DBLAS_LIBRARIES=${blas}/lib/libblas.so"
    "-DCBLAS_LIBRARIES=${blas}/lib/libcblas.so"
  ];

  buildInputs = [ pcre2 doxygen graphviz gperftools blas ];

  patches = [ ./bergamot.patch ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin
    cp app/bergamot $out/bin
    runHook postInstall
  '';

  meta = with lib; {
    description = "";
    homepage = "https://browser.mt/";
    license = licenses.mpl20;
    maintainers = with maintainers; [ kurnevsky ];
  };
}
