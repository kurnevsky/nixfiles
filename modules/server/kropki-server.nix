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
    rev = "117a85429698c84791c965fae8a78d2ee5a0078b";
    hash = "sha256-WHR5eaqsA6mZrEHYyFDFOy9ustl9mjHBLuEeZNrKsNU=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-Kq0db/XdaLA8GSGRCeZbYQYGKettGSwLzvCquoZLnPg=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
