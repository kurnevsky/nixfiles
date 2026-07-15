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
    rev = "e5907559cdf9de13346ff5b8f9c409d48b19c3aa";
    hash = "sha256-EwgP/ociWlP6KdP3uY3WLIIEa/ATDboz9ziIEfynHOI=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-klfk3wpXcrs67YvKnk6gNdyqd++6S48ljbqYmGyF71Y=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
