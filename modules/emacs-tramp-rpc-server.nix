{
  lib,
  rustPlatform,
  fetchFromGitHub,
  git,
}:

rustPlatform.buildRustPackage rec {
  pname = "tramp-rpc-server";
  version = "0.14.0";

  src = fetchFromGitHub {
    owner = "ArthurHeymans";
    repo = "emacs-tramp-rpc";
    rev = "v${version}";
    hash = "sha256-+Z7x/SwGrz/GmF74Qwj8i8xfIpEUS7rWYZO1Jlygtfc=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-Boo8T+q06fTGEoZP2uYS19AAURPLNcXPUmuAA/j67qg=";

  doCheck = false;

  meta = with lib; {
    description = "High-performance TRAMP backend using JSON-RPC instead of shell parsing";
    homepage = "https://github.com/ArthurHeymans/emacs-tramp-rpc";
    license = [ licenses.gpl3 ];
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "tramp-rpc-server";
  };
}
