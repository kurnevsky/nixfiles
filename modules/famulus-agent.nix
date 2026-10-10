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
    rev = "8d7abc4d6ec7632cb5b9e900631d0667a64a14d6";
    hash = "sha256-nNsRUHjeBdwYYqDPiaDahjgV0MyBRCifijj/Loq5Jlg=";
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
