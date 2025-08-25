{ pkgs, ... }:

pkgs.rustPlatform.buildRustPackage {
  pname = "pinenote-dbus-service";
  version = "0.2.5";

  src = pkgs.fetchFromGitHub {
    owner = "PNDeb";
    repo = "pinenote_dbus_service";
    rev = "83ed0186deb3ac908ccb2acc6deb2a8485832791";
    hash = "sha256-JgjjSL7Kg/HecOGy+Oja+3CIi04enfJpCI2TWsnS/o8=";
  };

  postPatch = ''
    substituteInPlace src/ebc_ioctl.rs \
      --replace-fail 'ptr_screen_content: *const u8,' 'ptr_screen_content: *const i8,'
  '';

  postInstall = ''
    install -D -m 0644 dbus_security_configuration/pinenote.conf $out/etc/dbus-1/system.d/pinenote.conf
  '';

  nativeBuildInputs = with pkgs; [ pkg-config ];

  buildInputs = with pkgs; [ dbus ];

  cargoHash = "sha256-1lUNe6Ik8noy1ucFxc/qdJz9OozwLdfdhBeKSL6wYWg=";

  meta = with pkgs.lib; {
    description = "A dbus daemon for controlling some driver aspects related to the Pine64 Pinenote";
    homepage = "https://github.com/PNDeb/pinenote_dbus_service";
    licence = licenses.asl20;
  };
}
