{
  lib,
  rustPlatform,
  fetchFromGitHub,
  git,
}:

rustPlatform.buildRustPackage rec {
  pname = "tramp-rpc-server";
  version = "0.15.0";

  src = fetchFromGitHub {
    owner = "ArthurHeymans";
    repo = "emacs-tramp-rpc";
    rev = "v${version}";
    hash = "sha256-ZqDzg1Pynklz6QMnEIpyy754iQoe1qGaSWRlrhxMG5k=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-zNDwZ8JpqYpbQhz3TXL03UEQNTXDVccSb6ShiQOXBtA=";

  doCheck = false;

  meta = with lib; {
    description = "High-performance TRAMP backend using JSON-RPC instead of shell parsing";
    homepage = "https://github.com/ArthurHeymans/emacs-tramp-rpc";
    license = [ licenses.gpl3 ];
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "tramp-rpc-server";
  };
}
