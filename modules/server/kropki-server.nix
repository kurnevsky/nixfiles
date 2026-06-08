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
    rev = "37cb048a4151ae9e647a83501e3f0e1b60e39b2b";
    hash = "sha256-FN+NShU++UJ1xEln/qwl5mnoFFVDtKOopO5QLwgC49A=";
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
