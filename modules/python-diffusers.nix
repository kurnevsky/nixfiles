{ python3Packages, fetchFromGitHub, ftfy, scipy, torch, huggingface-hub, regex
, importlib-metadata, ... }:

python3Packages.buildPythonPackage rec {
  pname = "diffusers";
  version = "0.21.4";
  format = "setuptools";

  src = fetchFromGitHub {
    owner = "huggingface";
    repo = "diffusers";
    rev = "refs/tags/v${version}";
    hash = "sha256-nyKZVnthADMFA2DVlCPLYJUf0R3fV++OzGhjHDJnMI0=";
  };

  propagatedBuildInputs = [ ftfy scipy ];

  buildInputs = [ torch huggingface-hub regex importlib-metadata ];

  # Many tests require internet access.
  doCheck = false;
}
