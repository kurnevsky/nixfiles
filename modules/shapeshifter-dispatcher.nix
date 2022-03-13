{ lib, buildGoModule, fetchFromGitHub, ... }:

buildGoModule rec {
  pname = "shapeshifter-dispatcher";
  version = "2.2.1";

  src = fetchFromGitHub {
    owner = "OperatorFoundation";
    repo = pname;
    rev = "v${version}";
    sha256 = "sha256-KSQAOoPH78jmdyZY6cDXXGyQRUeItAnMWyOk99GTJAU=";
  };

  vendorSha256 = "sha256-lYD63wScqCZaidDCWVANMqL2ciStKaiGLMb0AuU7rjQ=";

  doCheck = false;

  meta = with lib; {
    description =
      "The Shapeshifter project provides network protocol shapeshifting technology (also sometimes referred to as obfuscation).";
    homepage = "https://github.com/OperatorFoundation/shapeshifter-dispatcher";
    license = licenses.mit;
    maintainers = with maintainers; [ kurnevsky ];
    platforms = platforms.all;
  };
}
