{ lib, rustPlatform, fetchFromGitHub }:

rustPlatform.buildRustPackage {
  pname = "kropki-server";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "pointsgame";
    repo = "oppai-rs";
    rev = "c8295f741c654b82c9a542ae461bdf4ecd544df4";
    sha256 = "sha256-gVxKHyuZa2K5jzzM+KR84P8pqJyf+2uOczZ9/hr0jJk=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-TPLnPkOLBllIV47I0w2Z8KqV3xpzh1nDEuUCHcTNdkk=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
