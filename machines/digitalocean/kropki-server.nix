{ lib, rustPlatform, fetchFromGitHub }:

rustPlatform.buildRustPackage {
  pname = "kropki-server";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "pointsgame";
    repo = "oppai-rs";
    rev = "c09bee32eb70619786f785d2f31f98ecfdb04368";
    sha256 = "sha256-Ue+e8u59ECMdEWVAHTcqXcP/8Hzc/4/HZvypsNp9iE4=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-7TfmFO1TnYT/78lSCDpNSqyETKrlPDeb/xuDA8IXwzw=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
