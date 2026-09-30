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
    rev = "12ae71bcf4cdbc69617171c19f562ff90b56e1d9";
    hash = "sha256-gM4RquvOR7JxgFbr7GDZVTOwDloGZk3WeuS8gWHY6Vg=";
  };

  cargoHash = "sha256-YHnmLfiqah6+bwIBvxT8f7sZ3+l1magh84Jb+xu4NxA=";

  meta = with lib; {
    description = "A minimal terminal coding agent in Rust";
    homepage = "https://github.com/kurnevsky/famulus-agent";
    license = licenses.agpl3Only;
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "fa";
  };
}
