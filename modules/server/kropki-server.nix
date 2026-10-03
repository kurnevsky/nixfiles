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
    rev = "eb35c103e6ededc89995d33b0efb256b679ed738";
    hash = "sha256-YKrLnVW1cPHXNkGm0bxnxDB2uI+FOKeelHpu5GDu1tw=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-RuFbSfm7Orj10bgUXaZ30gXeXHGcqzjmNK3nShsPkC4=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
