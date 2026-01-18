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
    rev = "5247525ca27c468511fc057de7a90a09334d2204";
    hash = "sha256-D+kxLvhL8uyAVEfwE4JXpSrM1Uxv3iNPW46yWMc1ako=";
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
