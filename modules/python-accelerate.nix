{ python3Packages, fetchFromGitHub, pyyaml, packaging, numpy, pytorch, psutil
, ... }:

(python3Packages.buildPythonPackage rec {
  pname = "accelerate";
  version = "0.15.0";
  format = "setuptools";

  src = fetchFromGitHub {
    owner = "huggingface";
    repo = "accelerate";
    rev = "v${version}";
    hash = "sha256-agfbOaa+Nm10HZkd2Y7zR3R37n+vLNsxCyxZax6O3Lo=";
  };

  buildInputs = [ pyyaml packaging numpy pytorch ];

  propagatedBuildInputs = [ psutil ];

  doCheck = false;
})
