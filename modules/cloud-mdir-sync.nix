{ lib, fetchFromGitHub, python3Packages }:

python3Packages.buildPythonPackage rec {
  pname = "cloud-mdir-sync";
  version = "1";

  src = fetchFromGitHub {
    owner = "jgunthorpe";
    repo = "cloud_mdir_sync";
    rev = "58657778420637e329d887b5b95cb30a90d43d1b";
    sha256 = "sha256-6aP3wES9RF8lkDrezGGimGIFFwPmfbeBwGFxlviKdRw=";
  };

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
