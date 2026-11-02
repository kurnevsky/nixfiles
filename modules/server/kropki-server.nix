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
    rev = "3ebef7343e1311ff61e4dca94bf8f4c4e7586b96";
    hash = "sha256-gPAZ3cBIX7JTn6CEYwnY5+RpB9E+IJ/7o2SxJQfvcjE=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-CI9348SPXDyJqNGHOVFfoTO+T0xo1R9F2SUPhOxZYxI=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
