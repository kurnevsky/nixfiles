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
    rev = "f026c2bfd46ec9f06ddba61f357b7db9b8aa4c73";
    hash = "sha256-BsiOrrghogFj4eiy8K1kWt6zNLZBQZH8abI2Zs+HGX8=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-4QcUXC6T7h4YJZdOP2y/LwOKjH6gCuHMb/qbYd31hEU=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
