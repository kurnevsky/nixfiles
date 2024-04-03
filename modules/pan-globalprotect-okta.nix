{ fetchFromGitHub, python3Packages }:

python3Packages.buildPythonPackage rec {
  pname = "pan-globalprotect-okta";
  version = "1.0.0";

  format = "other";

  src = fetchFromGitHub {
    owner = "arthepsy";
    repo = "pan-globalprotect-okta";
    rev = "da771a46c6e21b15c1fe4a0325222097028cffc5";
    sha256 = "sha256-ptS++IhGeQjBuwK/VzODDwmFvWYljBlkL+52X8BuUS4=";
  };

  propagatedBuildInputs = with python3Packages; [ requests lxml pyotp ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src/gp-okta.py $out/bin/gp-okta
  '';

  doCheck = false;
}
