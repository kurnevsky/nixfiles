{ fetchFromGitHub, python3, python3Packages }:

python3Packages.buildPythonPackage rec {
  pname = "pan-globalprotect-okta";
  version = "1.0.0";

  format = "other";

  src = fetchFromGitHub {
    owner = "agriic";
    repo = "pan-globalprotect-okta";
    rev = "7128b2996f1c950a8ddcd1e13d82c5564e2f5864";
    sha256 = "sha256-cn2TsBjFtUhPq37VC6VqUfgafiBUvw1wqkMJV+Y1UXY=";
  };

  propagatedBuildInputs = with python3Packages; [ requests lxml pyotp ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src/gp-okta.py $out/bin/gp-okta
  '';

  doCheck = false;
}
