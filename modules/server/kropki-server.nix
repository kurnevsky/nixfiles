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
    rev = "2eeff7ff1353d2a47f79927d7cfb6d2238c12b17";
    hash = "sha256-gB6+2dIwMUOfL0SkfJlqzzv8A0Qjpp1/1YQG9DYjmII=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-DxXTs1V8Z0y5njI7IJFNyW0Qo3Tv56cvstDTKLR4LyQ=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
