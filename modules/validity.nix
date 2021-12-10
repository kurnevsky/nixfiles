{ pkgs, ... }:

let
  open-fprintd = pkgs.callPackage ./open-fprintd.nix { };
  python-validity = pkgs.callPackage ./python-validity.nix { };
in {
  environment.systemPackages = with pkgs; [ fprintd ];

  systemd = {
    packages = [ open-fprintd python-validity ];
    services.python3-validity.wantedBy = [ "default.target" ];
  };

  services.dbus.packages = [ open-fprintd python-validity ];

  security.pam.services = {
    sudo.fprintAuth = true;
    login.fprintAuth = true;
    xscreensaver.fprintAuth = true;
  };
}
