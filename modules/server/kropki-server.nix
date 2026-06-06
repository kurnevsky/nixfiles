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
    rev = "159861567d09ed1e678c81a52cc54dd60f5a4043";
    hash = "sha256-3Itgzn2r8cuhUw9cgN3XtGo1maNQXJ7tc1o/lSxuru8=";
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
