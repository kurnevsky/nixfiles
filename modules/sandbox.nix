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
  firefox = {
    name = "firefox";
    devs = [ "dri" ];
    camera = true;
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "bus" "gnupg" "pulse" ];
    etcs = [ "fonts" "pulse" "resolv.conf" "localtime" ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" ];
    setenvs = [{
      name = "SHELL";
      value = "/run/current-system/sw/bin/bash";
    }];
    unshare-net = false;
    ro-whitelist = [ "~/.password-store" "~/.config/gtk-3.0" "~/.Xauthority" ];
    whitelist = [
      "~/.mozilla"
      "~/.cache/mozilla/firefox"
      "~/Downloads"
      "~/.cache/fontconfig"
      "~/.config/pulse"
      "~/.gnupg"
    ];
  };
  chromium = {
    name = "chromium";
    devs = [ "dri" ];
    camera = true;
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    system-bus-socket = true;
    pams = [ "bus" "gnupg" "pulse" ];
    etcs = [ "fonts" "pulse" "resolv.conf" "localtime" "ssl" ];
    unsetenvs = [ "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.Xauthority" ];
    whitelist = [
      "~/.config/chromium"
      "~/.cache/chromium"
      "~/Downloads"
      "~/.cache/fontconfig"
      "~/.config/pulse"
    ];
    args = [ "--no-sandbox" ];
  };
  pidgin = {
    name = "pidgin";
    x11 = true;
    etcs = [ "fonts" "pulse" "resolv.conf" "localtime" "ssl" ];
    pams = [ "bus" "pulse" ];
    unshare-net = false;
    unsetenvs = [ "MAIL" "SHELL" ];
    ro-whitelist = [ "~/.Xauthority" "~/.gtkrc-2.0" ];
    whitelist = [ "~/.purple" "~/.config/pulse" ];
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
    x11 = true;
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
  vlc = {
    name = "vlc";
    devs = [ "dri" ];
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "bus" "pulse" ];
    etcs = [ "fonts" "pulse" ];
    unsetenvs = [ "MAIL" ];
    setenvs = [{
      name = "SHELL";
      value = "/run/current-system/sw/bin/bash";
    }];
    ro-whitelist = [ "~" ];
    whitelist =
      [ "~/.local/share/vlc" "~/.cache/fontconfig" "~/.config/pulse" ];
    blacklist = [ "~/.gnupg" "~/.ssh" ];
  };
  qtox = {
    name = "qtox";
    devs = [ "dri" ];
    camera = true;
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "pulse" ];
    etcs = [ "fonts" "pulse" "localtime" "resolv.conf" ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/qt5ct" "~/.Xauthority" ];
    whitelist = [ "~/.config/tox" "~/.cache/Tox" "~/.config/pulse" ];
  };
  tdesktop = {
    name = "telegram-desktop";
    devs = [ "dri" ];
    camera = true;
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "pulse" ];
    etcs = [ "fonts" "pulse" "localtime" "resolv.conf" ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/qt5ct" "~/.Xauthority" ];
    whitelist = [ "~/.local/share/TelegramDesktop" "~/.config/pulse" ];
  };
in {
  nixpkgs.overlays = [
    (self: super: {
      sandbox-seccomp = pkgs.callPackage ./sandbox-seccomp.nix { };
    })
    (self: super: {
      zip-natspec = super.zip.override { enableNLS = true; };
      unzip-natspec = super.unzip.override { enableNLS = true; };
    })
    (self: super: {
      deadbeef-sandboxed = sandbox super.deadbeef-with-plugins deadbeef;
      deadbeef-sandboxed-net =
        sandbox super.deadbeef-with-plugins (withNet deadbeef);
      p7zip-sandboxed = sandbox super.p7zip (archiver "7z");
      unrar-sandboxed = sandbox super.unrar (archiver "unrar");
      zip-natspec-sandboxed = sandbox super.zip-natspec (archiver "zip");
      unzip-natspec-sandboxed = sandbox super.unzip-natspec (archiver "unzip");
      mpv-sandboxed = sandbox super.mpv mpv;
      mpv-sandboxed-net = sandbox super.mpv (withNet mpv);
      vlc-sandboxed = sandbox super.vlc vlc;
      vlc-sandboxed-net = sandbox super.vlc (withNet vlc);
      firefox-sandboxed = sandbox super.firefox firefox;
      chromium-sandboxed = sandbox super.chromium chromium;
      pidgin-sandboxed = sandbox super.pidgin-with-plugins pidgin;
      qtox-sandboxed = sandbox super.qtox qtox;
      tdesktop-sandboxed = sandbox super.tdesktop tdesktop;
    })
  ];
}
