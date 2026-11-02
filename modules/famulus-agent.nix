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
    rev = "26106cb835b31f207d56c51b7d95ac31e8e353bd";
    hash = "sha256-R5mKvMnQDuanFMLLUG/eEe85rteOb4O1bOYjuy7SrX8=";
  };

  cargoHash = "sha256-ejPs58haYrojyUrnHFNXNfrpV7CdmY7eeT4yyFpi3gI=";

  meta = with lib; {
    description = "A minimal terminal coding agent in Rust";
    homepage = "https://github.com/kurnevsky/famulus-agent";
    license = licenses.agpl3Only;
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "fa";
  };
}
