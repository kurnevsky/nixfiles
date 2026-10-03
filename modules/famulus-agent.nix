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
    rev = "c7045a01b2644a32b57f4f8377434d8bc79ba096";
    hash = "sha256-tV6mcRcCxtN9Smzn/vzeOq+4/DQy/gac++GHoXiDa24=";
  };

  cargoHash = "sha256-Vsi7WXtrbnec8SSVeeN1m/c7YdJtdlBsQ7+RYdpXJYE=";

  meta = with lib; {
    description = "A minimal terminal coding agent in Rust";
    homepage = "https://github.com/kurnevsky/famulus-agent";
    license = licenses.agpl3Only;
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "fa";
  };
}
