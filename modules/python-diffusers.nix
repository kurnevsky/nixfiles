{ python3Packages, fetchFromGitHub, ftfy, scipy, pytorch, huggingface-hub, regex
, importlib-metadata, ... }:

python3Packages.buildPythonPackage rec {
  pname = "diffusers";
  version = "0.12.1";
  format = "setuptools";

  src = fetchFromGitHub {
    owner = "huggingface";
    repo = "diffusers";
    rev = "refs/tags/v${version}";
    hash = "sha256-1yQ98M9YeLOPa5pnTujo0I+RwhA/agJQLYJGPFR23Rc=";
  };

  propagatedBuildInputs = [ ftfy scipy ];

  buildInputs = [ pytorch huggingface-hub regex importlib-metadata ];

  doCheck = false;
}
