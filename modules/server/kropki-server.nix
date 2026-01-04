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
    rev = "eb983e0ebf7e0ecb413db0aeddee12f00283741a";
    hash = "sha256-e9K8lSmf7UOFlWKrrLUBgSAystSw60TacpnptET5GJQ=";
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
