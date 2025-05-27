{ inputs, python3Packages }:

python3Packages.buildPythonPackage rec {
  pname = "pan-globalprotect-okta";
  version = "1.0.0";

  format = "other";

  src = inputs.pan-globalprotect-okta;

  propagatedBuildInputs = with python3Packages; [
    requests
    lxml
    pyotp
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src/gp-okta.py $out/bin/gp-okta
  '';

  doCheck = false;
}
