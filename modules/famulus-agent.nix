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
    rev = "3964817fcc357c62f899a57189ef04a56e3b43a8";
    hash = "sha256-7VLVMNBbwUHYXEspewkueD5Y6ICajHIBGLU9SA/NCTs=";
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
