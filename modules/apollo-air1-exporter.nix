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
    rev = "4830bca9f9bf27f40af96b5c4b1dbefac36414a5";
    hash = "sha256-1rWN4q+GVp9GdktQFwM6tslphkkA9km2TdNk8NdPQr0=";
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
