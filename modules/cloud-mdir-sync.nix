{
  lib,
  fetchFromGitHub,
  python3Packages,
}:

python3Packages.buildPythonPackage rec {
  pname = "cloud-mdir-sync";
  version = "1";

  src = fetchFromGitHub {
    owner = "jgunthorpe";
    repo = "cloud_mdir_sync";
    rev = "5f94e58a043fef57be41c01c21ae5efb955098c6";
    sha256 = "sha256-AKlcHyj8F2oWySQzxYp4JbWP2BkKQOTSiBKeHX7htog=";
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
