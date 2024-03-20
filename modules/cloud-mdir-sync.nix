{ lib, fetchFromGitHub, python3Packages }:

python3Packages.buildPythonPackage rec {
  pname = "cloud-mdir-sync";
  version = "1";

  src = fetchFromGitHub {
    owner = "jgunthorpe";
    repo = "cloud_mdir_sync";
    rev = "c5c58b4723218b03b5fb8574a244d3497bd54992";
    sha256 = "sha256-G6XIg47JvQ3QGkzzVGGsnIXwy7QWB655vdPJDylvocI=";
  };

  postPatch = ''
    substituteInPlace setup.py \
      --replace "'pyasyncore'," ""
  '';

  propagatedBuildInputs = with python3Packages; [
    aiohttp
    keyring
    oauthlib
    pyinotify
  ];

  meta = with lib; {
    homepage = "https://github.com/jgunthorpe/cloud_mdir_sync";
    license = licenses.gpl2Plus;
  };
}
