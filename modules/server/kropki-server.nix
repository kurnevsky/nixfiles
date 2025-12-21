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
    rev = "09bc14813d69535059f04bdc43d1fae276161283";
    hash = "sha256-ZNIccjvqSiVlwy7T72ilwSXIkdubvhjlH4EpmerZFn0=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-CnS8WZ/2BRIT38LSLm+MAfAdo6TNGHY9p5AlnIohvzg=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
