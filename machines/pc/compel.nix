{
  python3Packages,
  fetchFromGitHub,
  pyparsing,
  torch,
  transformers,
  diffusers,
  ...
}:

(python3Packages.buildPythonPackage rec {
  pname = "compel";
  version = "2.0.2";
  format = "pyproject";

  src = fetchFromGitHub {
    owner = "damian0815";
    repo = "compel";
    rev = "v${version}";
    hash = "sha256-OHldDlHtxSs112rmy/DsZPV6TIhsmfAzcxH2rjJ9cR4=";
  };

  buildInputs = [
    pyparsing
    torch
    transformers
    diffusers
  ];

  propagatedBuildInputs = [ pyparsing ];

  doCheck = false;
})
