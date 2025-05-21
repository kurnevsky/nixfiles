{
  inputs,
  lib,
  python3Packages,
}:

python3Packages.buildPythonPackage rec {
  pname = "cloud-mdir-sync";
  version = "1";

  src = inputs.cloud-mdir-sync;

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
