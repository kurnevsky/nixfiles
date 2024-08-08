{ lib, rustPlatform, fetchFromGitHub }:

rustPlatform.buildRustPackage {
  pname = "kropki-server";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "pointsgame";
    repo = "oppai-rs";
    rev = "1d2d58a577ca3acb00d5e3c08eb081c0562ce0b4";
    sha256 = "sha256-UrUIW8KPTJOi1DcoDNZaE3OIg5gXvxGa7501t7Q5tTE=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-WCTyC0KcYxev5Mrhe0OoniN51HToENlp7TS2TIFHisw=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
