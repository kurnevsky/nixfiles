{
  lib,
  rustPlatform,
  fetchFromGitHub,
  git,
}:

rustPlatform.buildRustPackage rec {
  pname = "tramp-rpc-server";
  version = "0.13.1";

  src = fetchFromGitHub {
    owner = "ArthurHeymans";
    repo = "emacs-tramp-rpc";
    rev = "v${version}";
    hash = "sha256-8nMRbmPjn1182NuIXVe/3aYu0U+sNHVOnXPMwTZaIaU=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-tbkc5FHzkHuvGaWAycHNTw1UEiD/Cb4jNdSXdy65IxE=";

  doCheck = false;

  meta = with lib; {
    description = "High-performance TRAMP backend using JSON-RPC instead of shell parsing";
    homepage = "https://github.com/ArthurHeymans/emacs-tramp-rpc";
    license = [ licenses.gpl3 ];
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "tramp-rpc-server";
  };
}
