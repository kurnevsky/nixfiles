{ python3Packages, fetchFromGitHub, pyyaml, packaging, numpy, torch, psutil
, ... }:

(python3Packages.buildPythonPackage rec {
  pname = "accelerate";
  version = "0.23.0";
  format = "setuptools";

  src = fetchFromGitHub {
    owner = "huggingface";
    repo = "accelerate";
    rev = "v${version}";
    hash = "sha256-pFkEgE1NGLPBW1CeGU0RJr+1Nj/y58ZcljyOnJuR47A=";
  };

  buildInputs = [ pyyaml packaging numpy torch ];

  propagatedBuildInputs = [ psutil ];

  doCheck = false;
})
