{
  lib,
  rustPlatform,
  fetchFromGitHub,
  cacert,
}:

rustPlatform.buildRustPackage rec {
  pname = "famulus-agent";
  version = "0.0.0";

  nativeBuildInputs = [ cacert ];

  src = fetchFromGitHub {
    owner = "kurnevsky";
    repo = "famulus-agent";
    rev = "c60d650735ae5a536479adde68ed6ec9abe90508";
    hash = "sha256-qBANXideSIKgEpOTnA9apw1SntWsBcatRj58M1qeO3E=";
  };

  cargoHash = "sha256-qYCJT4abO684uKfITmUxT8jPRWfJfEdqp+1HwaIJLMU=";

  meta = with lib; {
    description = "A minimal terminal coding agent in Rust";
    homepage = "https://github.com/kurnevsky/famulus-agent";
    license = licenses.agpl3Only;
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "fa";
  };
}
