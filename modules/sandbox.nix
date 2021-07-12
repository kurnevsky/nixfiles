{ config, lib, pkgs, ... }:

let
  sandbox = pkgs.callPackage ./sandbox-bwrap.nix { };
  withNet = attrs:
    attrs // {
      target-name = attrs.name + "-net";
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
    whitelist = [ "~/" ];
    blacklist = [ "~/.gnupg/" "~/.ssh/" ];
  };
  viewer = name: {
    inherit name;
    x11 = true;
    etcs = [ "fonts" ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "XDG_RUNTIME_DIR" "MAIL" "SHELL" ];
    ro-whitelist = [ "~/" ];
    blacklist = [ "~/.gnupg/" "~/.ssh/" ];
  };
  deadbeef = {
    name = "deadbeef";
    extra-deps = with pkgs; [
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
    ];
    pams = [
      # Necessary for MPRIS2
      "bus"
      "pulse"
    ];
    etcs = [ "fonts" "pulse" ];
    x11 = true;
    unsetenvs = [ "MAIL" "SHELL" ];
    ro-whitelist = [ "~/" ];
    whitelist = [ "~/.config/pulse/" "~/.config/deadbeef/" ];
    blacklist = [ "~/.gnupg/" "~/.ssh/" ];
  };
  firefox = {
    name = "firefox";
    extra-deps = with pkgs; [ mesa_drivers ];
    devs = [ "dri" ];
    camera = true;
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "bus" "gnupg" "pulse" ];
    etcs = [
      "fonts"
      "pulse"
      "resolv.conf"
      "localtime"
      "ssl/certs/ca-certificates.crt"
    ];
    opengl = true;
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" ];
    setenvs = [{
      name = "SHELL";
      value = "/run/current-system/sw/bin/bash";
    }];
    unshare-net = false;
    ro-whitelist =
      [ "~/.password-store/" "~/.config/gtk-3.0/" "~/.Xauthority" ];
    whitelist = [
      "~/.mozilla/"
      "~/.cache/mozilla/firefox/"
      "~/Downloads/"
      "~/.cache/fontconfig/"
      "~/.config/pulse/"
      "~/.gnupg/"
    ];
  };
  chromium = {
    name = "chromium";
    extra-deps = with pkgs; [ mesa_drivers ];
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
    etcs = [
      "fonts"
      "pulse"
      "resolv.conf"
      "localtime"
      "ssl/certs/ca-certificates.crt"
    ];
    opengl = true;
    unsetenvs = [ "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.Xauthority" ];
    whitelist = [
      "~/.config/chromium/"
      "~/.cache/chromium/"
      "~/Downloads/"
      "~/.cache/fontconfig/"
      "~/.config/pulse/"
    ];
    args = [ "--no-sandbox" ];
  };
  pidgin = {
    name = "pidgin";
    extra-deps = with pkgs; [
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
    ];
    x11 = true;
    etcs = [
      "fonts"
      "pulse"
      "resolv.conf"
      "localtime"
      "ssl/certs/ca-certificates.crt"
    ];
    pams = [ "bus" "pulse" ];
    unshare-net = false;
    unsetenvs = [ "MAIL" "SHELL" ];
    ro-whitelist = [ "~/.Xauthority" "~/.gtkrc-2.0" ];
    whitelist = [ "~/.purple/" "~/.config/pulse/" ];
  };
  mpv = {
    name = "mpv";
    bin-sh = true;
    extra-deps = with pkgs; [
      coreutils-full
      xdg-utils
      xorg.xprop
      xscreensaver
      mesa_drivers
      wmctrl
      gawk
      xcb-client-id
    ];
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
    opengl = true;
    # xdg-screensaver creates a lockfile in /tmp
    shared-tmp = true;
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" ];
    setenvs = [{
      name = "SHELL";
      value = "/run/current-system/sw/bin/bash";
    }];
    ro-whitelist = [ "~/" ];
    whitelist = [ "~/.cache/fontconfig/" "~/.config/pulse/" ];
    blacklist = [ "~/.gnupg/" "~/.ssh/" ];
  };
  vlc = {
    name = "vlc";
    extra-deps = with pkgs; [ mesa_drivers ];
    devs = [ "dri" ];
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "bus" "pulse" ];
    etcs = [ "fonts" "pulse" ];
    opengl = true;
    unsetenvs = [ "MAIL" ];
    setenvs = [{
      name = "SHELL";
      value = "/run/current-system/sw/bin/bash";
    }];
    ro-whitelist = [ "~/" ];
    whitelist =
      [ "~/.local/share/vlc/" "~/.cache/fontconfig/" "~/.config/pulse/" ];
    blacklist = [ "~/.gnupg/" "~/.ssh/" ];
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
    pams = [ "bus" "pulse" ];
    etcs = [ "fonts" "pulse" "localtime" "resolv.conf" ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/qt5ct/" "~/.Xauthority" ];
    whitelist = [ "~/.config/tox/" "~/.cache/Tox/" "~/.config/pulse/" ];
  };
  toxic = {
    name = "toxic";
    extra-deps = with pkgs; [ glibcLocales ];
    devs = [ "dri" ];
    camera = true;
    x11 = true;
    pams = [ "pulse" ];
    etcs = [ "pulse" "localtime" "resolv.conf" ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.Xauthority" ];
    whitelist = [ "~/.config/tox/" "~/.config/pulse/" ];
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
    ro-whitelist = [ "~/.config/qt5ct/" "~/.Xauthority" ];
    whitelist = [ "~/.local/share/TelegramDesktop/" "~/.config/pulse/" ];
  };
  element-desktop = {
    name = "element-desktop";
    devs = [
      "dri"
      # Necessary for audio
      "snd"
    ];
    camera = true;
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "pulse" ];
    etcs = [
      "fonts"
      "pulse"
      "localtime"
      "resolv.conf"
      "ssl/certs/ca-certificates.crt"
    ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/gtk-3.0/" "~/.Xauthority" ];
    whitelist = [ "~/.config/Element/" "~/.config/pulse/" ];
  };
  qbittorrent = {
    name = "qbittorrent";
    extra-deps = with pkgs; [
      qt5ct
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
    ];
    devs = [ "dri" ];
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "bus" ];
    etcs = [ "fonts" "localtime" "resolv.conf" ];
    unsetenvs = [ "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/qt5ct/" "~/.Xauthority" ];
    whitelist = [
      "~/.local/share/data/qBittorrent/"
      "~/.config/qBittorrent/"
      "~/.cache/qBittorrent/"
      "~/Torrents/"
      "~/movies/"
    ];
  };
  ffmpeg = {
    name = "ffmpeg";
    devs = [ "dri" ];
    camera = true;
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    whitelist = [ "~/" ];
    blacklist = [ "~/.gnupg/" "~/.ssh/" ];
    unsetenvs = [
      "DBUS_SESSION_BUS_ADDRESS"
      "XDG_RUNTIME_DIR"
      "XAUTHORITY"
      "MAIL"
      "SHELL"
    ];
  };
  ffprobe = {
    name = "ffprobe";
    devs = [ "dri" ];
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    ro-whitelist = [ "~/" ];
    blacklist = [ "~/.gnupg/" "~/.ssh/" ];
    unsetenvs = [
      "DBUS_SESSION_BUS_ADDRESS"
      "XDG_RUNTIME_DIR"
      "XAUTHORITY"
      "MAIL"
      "SHELL"
    ];
  };
  wine = name: {
    inherit name;
    # coreutils-full is needed because it's system default stdenv
    # and wine has scripts that rely on stdenv being in PATH
    extra-deps = with pkgs; [ coreutils-full mesa_drivers ];
    devs = [ "dri" ];
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "bus" "pulse" ];
    etcs = [ "fonts" "localtime" ];
    opengl = true;
    unsetenvs = [ "MAIL" ];
    setenvs = [{
      name = "SHELL";
      value = "/run/current-system/sw/bin/bash";
    }];
    unshare-cgroup = false;
    seccomp = false;
    ro-whitelist = [ "~/.Xauthority" ];
    whitelist = [
      "\${WINEPREFIX:-~/.wine/}"
      "~/.cache/wine/"
      "~/.cache/winetricks/"
      "~/.config/pulse/"
    ];
  };
  libreoffice = name: {
    inherit name;
    # coreutils-full, gnugrep, gnused are needed because it's
    # system default stdenv and libreoffice has scripts that rely
    # on stdenv being in PATH
    extra-deps = with pkgs; [ coreutils-full gnugrep gnused ];
    x11 = true;
    etcs = [ "fonts" "localtime" "pulse" "passwd" ];
    pams = [ "bus" "pulse" ];
    unsetenvs = [ "MAIL" "SHELL" ];
    whitelist = [ "~/" ];
    blacklist = [ "~/.gnupg/" "~/.ssh/" ];
  };
  tor-browser = {
    name = "tor-browser";
    x11 = true;
    unsetenvs = [ "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.Xauthority" ];
    whitelist = [ "~/.local/share/tor-browser/" ];
  };
  zoom = {
    name = "zoom";
    extra-deps = with pkgs; [ mesa_drivers ];
    devs = [ "dri" ];
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    camera = true;
    x11 = true;
    system-bus-socket = true;
    etcs = [ "fonts" "pulse" "localtime" "resolv.conf" ];
    opengl = true;
    pams = [ "bus" "pulse" ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" ];
    setenvs = [{
      name = "SHELL";
      value = "/run/current-system/sw/bin/bash";
    }];
    unshare-net = false;
    ro-whitelist = [ "~/.Xauthority" ];
    whitelist = [
      "~/.zoom/"
      "~/.cache/zoom/"
      "~/.config/zoomus.conf"
      "~/.config/pulse/"
    ];
  };
  skypeforlinux = {
    name = "skypeforlinux";
    devs = [
      "dri"
      # Necessary for audio
      "snd"
    ];
    camera = true;
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "pulse" ];
    etcs = [
      "fonts"
      "pulse"
      "localtime"
      "resolv.conf"
      "ssl/certs/ca-certificates.crt"
    ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/gtk-3.0/" "~/.Xauthority" ];
    whitelist = [ "~/.config/skypeforlinux/" "~/.config/pulse/" ];
  };
in {
  nixpkgs.overlays = [
    (self: super: {
      sandbox-seccomp = pkgs.callPackage ./sandbox-seccomp.nix { };
      xcb-client-id = pkgs.callPackage ./xcb-client-id.nix { };
    })
    (self: super: {
      zip-natspec = super.zip.override { enableNLS = true; };
      unzip-natspec = super.unzip.override { enableNLS = true; };
      wine-staging-full =
        super.wineWowPackages.full.override { wineRelease = "staging"; };
    })
    (self: super: {
      deadbeef-sandboxed = pkgs.symlinkJoin {
        name = "deadbeef";
        paths = [
          (sandbox super.deadbeef-with-plugins deadbeef)
          (sandbox super.deadbeef-with-plugins (withNet deadbeef))
        ];
      };
      p7zip-sandboxed = pkgs.symlinkJoin {
        name = "p7zip";
        paths = [
          (sandbox super.p7zip (archiver "7z"))
          (sandbox super.p7zip (archiver "7za"))
          (sandbox super.p7zip (archiver "7zr"))
        ];
      };
      unrar-sandboxed = sandbox super.unrar (archiver "unrar");
      zip-natspec-sandboxed = sandbox super.zip-natspec (archiver "zip");
      unzip-natspec-sandboxed = sandbox super.unzip-natspec (archiver "unzip");
      mpv-sandboxed = pkgs.symlinkJoin {
        name = "mpv";
        paths = [ (sandbox super.mpv mpv) (sandbox super.mpv (withNet mpv)) ];
      };
      vlc-sandboxed = pkgs.symlinkJoin {
        name = "vlc";
        paths = [ (sandbox super.vlc vlc) (sandbox super.vlc (withNet vlc)) ];
      };
      firefox-sandboxed = sandbox super.firefox firefox;
      chromium-sandboxed = sandbox super.chromium chromium;
      pidgin-sandboxed = sandbox super.pidgin-with-plugins pidgin;
      qtox-sandboxed = sandbox super.qtox qtox;
      toxic-sandboxed = sandbox super.toxic toxic;
      tdesktop-sandboxed = sandbox super.tdesktop tdesktop;
      element-desktop-sandboxed = sandbox super.element-desktop element-desktop;
      qbittorrent-sandboxed = sandbox super.qbittorrent qbittorrent;
      feh-sandboxed = sandbox super.feh (viewer "feh");
      imv-sandboxed = sandbox super.imv ((viewer "imv") // {
        opengl = true;
        extra-deps = with pkgs; [ mesa_drivers ];
      });
      zathura-sandboxed = sandbox super.zathura ((viewer "zathura") // {
        whitelist = [ "~/.local/share/zathura/" "~/Print/" ];
      });
      ffmpeg-full-sandboxed = pkgs.symlinkJoin {
        name = "ffmpeg";
        paths = [
          (sandbox super.ffmpeg-full ffmpeg)
          (sandbox super.ffmpeg-full ffprobe)
          super.ffmpeg-full
        ];
      };
      wine-staging-sandboxed = pkgs.symlinkJoin {
        name = "wine";
        paths = [
          (sandbox super.wineWowPackages.staging (wine "wine"))
          (sandbox super.wineWowPackages.staging (withNet (wine "wine")))
          (sandbox super.wineWowPackages.staging (wine "winecfg"))
        ];
      };
      wine-staging-full-sandboxed = pkgs.symlinkJoin {
        name = "wine";
        paths = [
          (sandbox super.wine-staging-full (wine "wine"))
          (sandbox super.wine-staging-full (withNet (wine "wine")))
          (sandbox super.wine-staging-full (wine "winecfg"))
        ];
      };
      libreoffice-fresh-sandboxed = pkgs.symlinkJoin {
        name = "libreoffice";
        paths = [
          (sandbox super.libreoffice-fresh (libreoffice "soffice"))
          (sandbox super.libreoffice-fresh (libreoffice "libreoffice"))
        ];
      };
      tor-browser-bundle-bin-wrapped =
        sandbox super.tor-browser-bundle-bin tor-browser;
      zoom-us-sandboxed = sandbox super.zoom-us zoom;
      skypeforlinux-sandboxed = sandbox super.skypeforlinux skypeforlinux;
    })
  ];
}
