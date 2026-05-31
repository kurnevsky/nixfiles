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
    rev = "127d2bcb9fc81587e419d1a9ac85a53bba5fbe27";
    hash = "sha256-ByLRic3pK/coRQEa63wZ4EoQainFlDdPAapMoET6yDA=";
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
