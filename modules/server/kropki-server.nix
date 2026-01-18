{
  lib,
  rustPlatform,
  fetchFromGitHub,
}:

rustPlatform.buildRustPackage {
  pname = "kropki-server";
  version = "0.0.1";

  src = fetchFromGitHub {
    owner = "pointsgame";
    repo = "oppai-rs";
    rev = "0807929c71405392caf755445bd2757a4993727a";
    hash = "sha256-A18a86Pftw86PGMLBFfMZSHPZqUCU3sguGpLwXeV8VQ=";
  };

  buildAndTestSubdir = "server";

  cargoHash = "sha256-JlfOTSU0Cbn1EABl3dxIwNlC4nqC/wd5E07I08SYRdA=";

  meta = with lib; {
    description = "Kropki server";
    homepage = "https://github.com/pointsgame/oppai-rs";
    license = [ licenses.agpl3Plus ];
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "kropki";
  };
}
