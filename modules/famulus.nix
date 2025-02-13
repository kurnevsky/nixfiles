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

  useFetchCargoVendor = true;

  cargoHash = "sha256-rmGOLhyP1JctzBlYsmv3oqc2HtkdURB6IueDC3/bHAs=";

  meta = with lib; {
    description = "LSP server integrating LLMs";
    homepage = "https://github.com/kurnevsky/famulus";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "famulus";
  };
}
