{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage rec {
  pname = "famulus";
  version = "0.0.5";

  src = fetchFromGitHub {
    owner = "kurnevsky";
    repo = "famulus";
    rev = "v${version}";
    hash = "sha256-Vu8Bsm6oaDNLbrYpT+5ChMciDKl8Urdss/9M0aIoK14=";
  };

  cargoHash = "sha256-WyZuo2Abl4adZcZCizWiqOSt5cz+ozgzZWPAKz8b0XU=";

  meta = with lib; {
    description = "LSP server integrating LLMs";
    homepage = "https://github.com/kurnevsky/famulus";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "famulus";
  };
}
