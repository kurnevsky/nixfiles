{
  lib,
  rustPlatform,
  fetchFromGitHub,
  git,
}:

rustPlatform.buildRustPackage rec {
  pname = "apollo-air1-exporter";
  version = "0.0.10";

  src = fetchFromGitHub {
    owner = "kurnevsky";
    repo = "apollo-air1-exporter";
    rev = "c6a66c8ca50bb978690842de575a6a5c5b753648";
    hash = "sha256-HJo+i7hUPhKpQr8exyBhXSWcOogshSY0mak8QelaafA=";
  };

  cargoHash = "sha256-QdZ4TV0ZeN+RjggeoFeoHZ/mtbNUFRUqX8xU6tDKQLM=";

  meta = with lib; {
    description = "Prometheus exporter for Apollo AIR-1 air quality monitors";
    homepage = "https://github.com/kurnevsky/apollo-air1-exporter";
    license = [ licenses.mit ];
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "apollo-air1-exporter";
  };
}
