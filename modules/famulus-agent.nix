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
    rev = "af8f4b85da68c2f8425a618c4a4d8e084a0c7b17";
    hash = "sha256-XuY8x53ZVKzqUXsmTyY/7Oc01tYWe+QqdGf7t/0saJk=";
  };

  cargoHash = "sha256-3cpYdTU3wLgzHJS0yobuanxh4AnoxPhtnF63jze1bRg=";

  meta = with lib; {
    description = "A minimal terminal coding agent in Rust";
    homepage = "https://github.com/kurnevsky/famulus-agent";
    license = licenses.agpl3Only;
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "fa";
  };
}
