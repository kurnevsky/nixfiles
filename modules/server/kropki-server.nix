{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage {
  pname = "kropki-server";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "pointsgame";
    repo = "oppai-rs";
    rev = "cc7b6c5ed9fbc25faf12a59d79b7295ba798eb19";
    hash = "sha256-Bl91WqU15HRPNfDBfKKpFiwr7bX6qD5F/W8XDMVS8c4=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-JlfOTSU0Cbn1EABl3dxIwNlC4nqC/wd5E07I08SYRdA=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
