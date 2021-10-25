{ pkgs, ... }:

let
  open-fprintd = pkgs.callPackage ./open-fprintd.nix { };
  python-validity = pkgs.callPackage ./python-validity.nix { };
in {
  environment.systemPackages = with pkgs; [ fprintd ];

  systemd.packages = [ open-fprintd python-validity ];

  services.dbus.packages = [ open-fprintd python-validity ];

  security.pam.services.sudo.fprintAuth = true;
  security.pam.services.login.fprintAuth = true;
  security.pam.services.xscreensaver.fprintAuth = true;
}
