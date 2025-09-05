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
    rev = "a10d5065bc8c4730c7820986473b8c219ce35a58";
    hash = "sha256-gePQEx+V58Y6Ve7WvJVPhYj2unT0RIdmn0qmdAwRjcA=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-+bO2IwE3AF3XfFI2ZfX3OvSoW1ZvVbCvnS5xBXwP/+o=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
