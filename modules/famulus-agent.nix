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
    rev = "bd5063255ead4a65f05456387bcd2b8c1d1c1c34";
    hash = "sha256-FLnrKkdTFC2yHZyo+FEu2vxqDVfdX7qz4LH78NSuZ8Y=";
  };

  cargoHash = "sha256-WYaXAXrPSL2I2DImSGI204CgX55bU4MX9gLXL+S1Khc=";

  meta = with lib; {
    description = "A minimal terminal coding agent in Rust";
    homepage = "https://github.com/kurnevsky/famulus-agent";
    license = licenses.agpl3Only;
    platforms = platforms.linux;
    maintainers = with maintainers; [ kurnevsky ];
    mainProgram = "fa";
  };
}
