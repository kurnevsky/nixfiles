{
  lib,
  rustPlatform,
  fetchFromGitHub,
  cacert,
  pkg-config,
  aws-lc,
}:

rustPlatform.buildRustPackage rec {
  pname = "famulus-agent";
  version = "0.0.0";

  nativeBuildInputs = [
    cacert
    pkg-config
  ];

  buildInputs = [ aws-lc ];

  src = fetchFromGitHub {
    owner = "kurnevsky";
    repo = "famulus-agent";
    rev = "13f3ebab9fbda4b490b028b825fb9baaf3cd75d2";
    hash = "sha256-2AnBkAWcrjGu681PxDqgXnUFJ71Ins/1GZOWyCVRfRo=";
  };

  cargoHash = "sha256-00gH+tzEwBRHCT8aaFKLFDssN0Bw/S77fs6ar9SVysI=";

  meta = with lib; {
    description = "A minimal terminal coding agent in Rust";
    homepage = "https://github.com/kurnevsky/famulus-agent";
    license = licenses.agpl3Only;
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "fa";
  };
}
