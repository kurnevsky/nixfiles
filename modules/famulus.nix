{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage rec {
  pname = "famulus";
  version = "0.0.4";

  src = fetchFromGitHub {
    owner = "kurnevsky";
    repo = "famulus";
    rev = "v${version}";
    hash = "sha256-cE3PVZOGNpMi+m9vk4fEtZSo+IOINZiG2lEQAmNWMAQ=";
  };

  cargoHash = "sha256-sUBwiCW0rYhIsS7oY9qkkuBFKJaxLDqg+7+5mhMNTKg=";

  meta = with lib; {
    description = "LSP server integrating LLMs";
    homepage = "https://github.com/kurnevsky/famulus";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "famulus";
  };
}
