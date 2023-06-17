{ buildPythonApplication, fetchFromGitHub, numpy, pyside6, setuptools, ... }:

buildPythonApplication rec {
  pname = "openhrv";
  version = "0.2.0";
  format = "pyproject";

  src = fetchFromGitHub {
    owner = "JanCBrammer";
    repo = "OpenHRV";
    rev = "42434bff8753bfc6b284643ac6b0c99f2e504673";
    hash = "sha256-6AZJcaWpSlEN0QyAQBuf0M8MS/Axk+gxKxQGxhsuV/Y=";
  };

  postPatch = ''
    substituteInPlace pyproject.toml \
      --replace '"PySide6 >= 6.4.0.1",' ""
  '';

  buildInputs = [ setuptools ];

  propagatedBuildInputs = [ numpy pyside6 ];

  doCheck = false;
}
