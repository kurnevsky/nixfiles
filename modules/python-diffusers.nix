{ python3Packages, fetchFromGitHub, ftfy, scipy, pytorch, huggingface-hub, regex
, importlib-metadata, ... }:

python3Packages.buildPythonPackage rec {
  pname = "diffusers";
  version = "0.14.0";
  format = "setuptools";

  src = fetchFromGitHub {
    owner = "huggingface";
    repo = "diffusers";
    rev = "refs/tags/v${version}";
    hash = "sha256-2Su4TWsUwAyWxtIJwTAZf36u/k5HRM4dc0WQUyTPlTM=";
  };

  propagatedBuildInputs = [ ftfy scipy ];

  buildInputs = [ pytorch huggingface-hub regex importlib-metadata ];

  # Many tests require internet access.
  doCheck = false;
}
