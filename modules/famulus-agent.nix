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
    rev = "6ce3787b8d37261bf810e1908526015ee0e24f93";
    hash = "sha256-GmiNMkRZLUqPemxE+WUI3hxXSjhcugBHQcCXcGOcGF8=";
  };

  cargoHash = "sha256-Co8NUcBvsPdG4PgazvofFk3mIqRXA9Hlhv3bluI2Mdg=";

  meta = with lib; {
    description = "A minimal terminal coding agent in Rust";
    homepage = "https://github.com/kurnevsky/famulus-agent";
    license = licenses.agpl3Only;
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "fa";
  };
}
