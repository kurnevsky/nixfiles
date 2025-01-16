{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage rec {
  pname = "famulus";
  version = "0.0.3";

  src = fetchFromGitHub {
    owner = "kurnevsky";
    repo = "famulus";
    rev = "v${version}";
    hash = "sha256-F0Sd5JtKuKYpWVDUHZa4rpA836vOi+USRmaFTQFUiys=";
  };

  cargoHash = "sha256-iVAfoA+CoE7NhQkvR6ccFRTTdTRSnL6hVL+Wd9EF9oA=";

  meta = with lib; {
    description = "LSP server integrating LLMs";
    homepage = "https://github.com/kurnevsky/famulus";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "famulus";
  };
}
