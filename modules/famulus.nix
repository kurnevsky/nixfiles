{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage rec {
  pname = "famulus";
  version = "0.0.6";

  src = fetchFromGitHub {
    owner = "kurnevsky";
    repo = "famulus";
    rev = "v${version}";
    hash = "sha256-CHBUul0aKc0bXp7r28gNm9LWDlkHlgBkpP7GuT2aAb0=";
  };

  cargoHash = "sha256-ZyvvxCwQQeKwybMFryZyI1FonGGDi3EE2ckOBfjn1q4=";

  meta = with lib; {
    description = "LSP server integrating LLMs";
    homepage = "https://github.com/kurnevsky/famulus";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "famulus";
  };
}
