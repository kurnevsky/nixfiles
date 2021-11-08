{ fetchFromGitHub, python3, python3Packages }:

python3Packages.buildPythonPackage rec {
  pname = "pan-globalprotect-okta";
  version = "1.0.0";

  format = "other";

  src = fetchFromGitHub {
    owner = "arthepsy";
    repo = "pan-globalprotect-okta";
    rev = "f88beb9d076c3371dc4697cea0ec3b7a85d169d5";
    sha256 = "sha256-Y8S1ua+TkjQ59tMQYwercJfHKzX0KyOi2oN2Ap/ne6I=";
  };

  propagatedBuildInputs = with python3Packages; [ requests lxml pyotp ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src/gp-okta.py $out/bin/gp-okta
  '';

  doCheck = false;
}
