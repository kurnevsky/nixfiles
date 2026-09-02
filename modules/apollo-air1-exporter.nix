{
  lib,
  rustPlatform,
  fetchFromGitHub,
  git,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "apollo-air1-exporter";
  version = "0.0.12";

  src = fetchFromGitHub {
    owner = "rvben";
    repo = "apollo-air1-exporter";
    tag = "v${finalAttrs.version}";
    hash = "sha256-rmKuxwB3r71a8A04qX0OQxy9RtUXywO2URAmfSJSNNM=";
  };

  cargoHash = "sha256-yk8aIGcFKApGM/4gXIf7mMcdGSGiFpqs7T+2om2P+gI=";

  meta = with lib; {
    description = "Prometheus exporter for Apollo AIR-1 air quality monitors";
    homepage = "https://github.com/kurnevsky/apollo-air1-exporter";
    license = [ licenses.mit ];
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "apollo-air1-exporter";
  };
})
