{
  lib,
  rustPlatform,
  fetchFromGitHub,
  git,
}:

rustPlatform.buildRustPackage rec {
  pname = "tramp-rpc-server";
  version = "0.3.0";

  src = fetchFromGitHub {
    owner = "ArthurHeymans";
    repo = "emacs-tramp-rpc";
    rev = "v${version}";
    hash = "sha256-9RFRYF5N1JMOQfDYnBmhk8vOKmdegGRokFYhpuFRPHo=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-9eSDmh+ZoPDUB99hel/bNFyd0D3y6dKT37eWfue60bc=";

  meta = with lib; {
    description = "High-performance TRAMP backend using JSON-RPC instead of shell parsing";
    homepage = "https://github.com/ArthurHeymans/emacs-tramp-rpc";
    license = [ licenses.gpl3 ];
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "tramp-rpc-server";
  };
}
