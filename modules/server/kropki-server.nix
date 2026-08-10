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
    rev = "1e1503f9390cec95c23b4e610f0bbc2f071510ec";
    hash = "sha256-cAydRjVgAgpZpDvTCRK0xxWR+L0uZHAQV0Ri7iRDSrU=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-RuFbSfm7Orj10bgUXaZ30gXeXHGcqzjmNK3nShsPkC4=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
