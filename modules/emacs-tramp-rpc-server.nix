{
  lib,
  rustPlatform,
  fetchFromGitHub,
  git,
}:

rustPlatform.buildRustPackage rec {
  pname = "tramp-rpc-server";
  version = "0.16.0";

  src = fetchFromGitHub {
    owner = "ArthurHeymans";
    repo = "emacs-tramp-rpc";
    rev = "v${version}";
    hash = "sha256-kmb0tolO3E/cXqvdzTUQFKPGgDXSO0uN+98RXvmMvBk=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-o+JyVmN8KzIiokr9UKpxwP+cSUf72RtH+/m0Rfck78g=";

  doCheck = false;

  meta = with lib; {
    description = "High-performance TRAMP backend using JSON-RPC instead of shell parsing";
    homepage = "https://github.com/ArthurHeymans/emacs-tramp-rpc";
    license = [ licenses.gpl3 ];
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "tramp-rpc-server";
  };
}
