{ pkgs, ... }:

pkgs.rustPlatform.buildRustPackage rec {
  pname = "pinenote-service";
  version = "1.0.1";

  src = pkgs.fetchFromSourcehut {
    owner = "~phantomas";
    repo = pname;
    rev = "v${version}";
    hash = "sha256-+F84gKDexWL9AcTFEj/JVJSdmT9o+6/ahogniz7N/lg=";
  };

  cargoHash = "sha256-A/EzXV3qSSBIyXe0R6XBzCXiuNJgFSS8VRDcDZIq5WQ=";

  meta = with pkgs.lib; {
    description = "PineNote Service aims to be a central, dbus-aware service to manage various PineNote specific configurations.";
    homepage = "https://git.sr.ht/~phantomas/pinenote-service/tree";
    licence = licenses.gpl3only;
  };
}
