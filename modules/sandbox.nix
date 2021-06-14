{ config, lib, pkgs, ... }:

let
  sandbox = pkgs.callPackage ./sandbox-bwrap.nix { };
  withNet = attrs:
    attrs // {
      unshare-net = false;
      etcs = attrs.etcs ++ [ "resolv.conf" ];
    };
  archiver = name: {
    inherit name;
    unsetenvs = [
      "DBUS_SESSION_BUS_ADDRESS"
      "XDG_RUNTIME_DIR"
      "XAUTHORITY"
      "MAIL"
      "SHELL"
    ];
    whitelist = [ "~" ];
    blacklist = [ "~/.gnupg" "~/.ssh" ];
  };
  deadbeef = {
    name = "deadbeef";
    pams = [
      # Necessary for MPRIS2
      "bus"
      "pulse"
    ];
    etcs = [ "fonts" "pulse" ];
    x11 = true;
    unsetenvs = [ "MAIL" "SHELL" ];
    ro-whitelist = [ "~" ];
    whitelist = [ "~/.config/pulse" "~/.config/deadbeef" ];
    blacklist = [ "~/.gnupg" "~/.ssh" ];
  };
  mpv = {
    name = "mpv";
    # unshare-pid breaks xdg-screensaver in a way that it can't detect
    # process termination and therefore might not enable screensaver
    unshare-pid = false;
    devs = [ "dri" ];
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    pams = [ "pulse" ];
    etcs = [ "fonts" "pulse" ];
    # xdg-screensaver creates a lockfile in /tmp
    shared-tmp = true;
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" ];
    setenvs = [{
      name = "SHELL";
      value = "/run/current-system/sw/bin/bash";
    }];
    ro-whitelist = [ "~" ];
    whitelist = [ "~/.cache/fontconfig" "~/.config/pulse" ];
    blacklist = [ "~/.gnupg" "~/.ssh" ];
  };
in {
  nixpkgs.overlays = [
    (self: super: rec {
      deadbeef-sandboxed = sandbox super.deadbeef-with-plugins deadbeef;
      deadbeef-sandboxed-net =
        sandbox super.deadbeef-with-plugins (withNet deadbeef);
      p7zip-sandboxed = sandbox super.p7zip (archiver "7z");
      unrar-sandboxed = sandbox super.unrar (archiver "unrar");
      zip-natspec = super.zip.override { enableNLS = true; };
      unzip-natspec = super.unzip.override { enableNLS = true; };
      zip-natspec-sandboxed = sandbox zip-natspec (archiver "zip");
      unzip-natspec-sandboxed = sandbox unzip-natspec (archiver "unzip");
      mpv-sandboxed = sandbox super.mpv mpv;
      mpv-sandboxed-net = sandbox super.mpv (withNet mpv);
    })
  ];
}
