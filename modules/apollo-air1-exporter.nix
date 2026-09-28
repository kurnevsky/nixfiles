{
  lib,
  rustPlatform,
  fetchFromGitHub,
  git,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "apollo-air1-exporter";
  version = "0.0.13";

  src = fetchFromGitHub {
    owner = "rvben";
    repo = "apollo-air1-exporter";
    tag = "v${finalAttrs.version}";
    hash = "sha256-JHSWSSCYmMT6wUA5vkCwFNSv6AdbxJoYnkGBowld7ZQ=";
  };

  cargoHash = "sha256-3gRQK9g8OSxSUCVlHKjmb35D1TmrYlgs3w1hN1WAvbA=";

  meta = with lib; {
    description = "Prometheus exporter for Apollo AIR-1 air quality monitors";
    homepage = "https://github.com/rvben/apollo-air1-exporter";
    license = [ licenses.mit ];
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "apollo-air1-exporter";
  };
})
