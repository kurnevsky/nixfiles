{ config, lib, pkgs, ... }:

let
  sandbox = pkgs.callPackage ./sandbox-bwrap.nix { };
  withFonts = attrs:
    attrs // {
      extra-deps = (attrs.extra-deps or [ ])
        ++ config.fonts.fontconfig.confPackages;
      etcs = (attrs.etcs or [ ]) ++ [ "fonts" ];
    };
  withNet = attrs:
    attrs // {
      target-name = attrs.name + "-net";
      unshare-net = false;
      etcs = (attrs.etcs or [ ]) ++ [ "resolv.conf" ];
    };
  withOpengl = attrs:
    attrs // {
      extra-deps = with config.hardware.opengl;
        (attrs.extra-deps or [ ]) ++ [ package ] ++ extraPackages;
      opengl = true;
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
  viewer = name:
    withFonts {
      inherit name;
      x11 = true;
      unsetenvs =
        [ "DBUS_SESSION_BUS_ADDRESS" "XDG_RUNTIME_DIR" "MAIL" "SHELL" ];
      ro-whitelist = [ "~/" ];
      blacklist = [ "~/.gnupg/" "~/.ssh/" ];
    };
  deadbeef = withFonts {
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
    etcs = [ "pulse" ];
    x11 = true;
    unsetenvs = [ "MAIL" "SHELL" ];
    ro-whitelist = [ "~/" ];
    whitelist = [ "~/.config/pulse/" "~/.config/deadbeef/" ];
    blacklist = [ "~/.gnupg/" "~/.ssh/" ];
  };
  firefox = lib.pipe {
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
    etcs =
      [ "pulse" "resolv.conf" "localtime" "ssl/certs/ca-certificates.crt" ];
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
  } [ withFonts withOpengl ];
  chromium = lib.pipe {
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
    etcs =
      [ "pulse" "resolv.conf" "localtime" "ssl/certs/ca-certificates.crt" ];
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
  } [ withFonts withOpengl ];
  pidgin = withFonts {
    name = "pidgin";
    extra-deps = with pkgs; [
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
    ];
    x11 = true;
    etcs =
      [ "pulse" "resolv.conf" "localtime" "ssl/certs/ca-certificates.crt" ];
    pams = [ "bus" "pulse" ];
    unshare-net = false;
    unsetenvs = [ "MAIL" "SHELL" ];
    ro-whitelist = [ "~/.Xauthority" "~/.gtkrc-2.0" ];
    whitelist = [ "~/.purple/" "~/.config/pulse/" ];
  };
  mpv = lib.pipe {
    name = "mpv";
    bin-sh = true;
    extra-deps = with pkgs; [
      coreutils-full
      xdg-utils
      xorg.xprop
      xscreensaver
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
    etcs = [ "pulse" ];
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
  } [ withFonts withOpengl ];
  vlc = lib.pipe {
    name = "vlc";
    devs = [ "dri" ];
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "bus" "pulse" ];
    etcs = [ "pulse" ];
    unsetenvs = [ "MAIL" ];
    setenvs = [{
      name = "SHELL";
      value = "/run/current-system/sw/bin/bash";
    }];
    ro-whitelist = [ "~/" ];
    whitelist =
      [ "~/.local/share/vlc/" "~/.cache/fontconfig/" "~/.config/pulse/" ];
    blacklist = [ "~/.gnupg/" "~/.ssh/" ];
  } [ withFonts withOpengl ];
  qtox = withFonts {
    name = "qtox";
    extra-deps = with pkgs; [
      qt5ct
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
    ];
    devs = [ "dri" ];
    camera = true;
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "bus" "pulse" ];
    etcs = [ "pulse" "localtime" "resolv.conf" ];
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
  tdesktop = withFonts {
    name = "telegram-desktop";
    devs = [ "dri" ];
    camera = true;
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "bus" "pulse" ];
    etcs = [ "pulse" "localtime" "resolv.conf" ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/qt5ct/" "~/.Xauthority" ];
    whitelist = [ "~/.local/share/TelegramDesktop/" "~/.config/pulse/" ];
  };
  element-desktop = withFonts {
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
    etcs =
      [ "pulse" "localtime" "resolv.conf" "ssl/certs/ca-certificates.crt" ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/gtk-3.0/" "~/.Xauthority" ];
    whitelist = [ "~/.config/Element/" "~/.config/pulse/" ];
  };
  qbittorrent = withFonts {
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
    etcs = [ "localtime" "resolv.conf" ];
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
  wine = name:
    lib.pipe {
      inherit name;
      # coreutils-full is needed because it's system default stdenv
      # and wine has scripts that rely on stdenv being in PATH
      extra-deps = with pkgs; [ coreutils-full ];
      devs = [ "dri" ];
      syses = [
        # Necessary for hardware acceleration
        "dev"
        "devices"
      ];
      x11 = true;
      pams = [ "bus" "pulse" ];
      etcs = [ "localtime" ];
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
    } [ withFonts withOpengl ];
  libreoffice = name:
    withFonts {
      inherit name;
      # coreutils-full, gnugrep, gnused are needed because it's
      # system default stdenv and libreoffice has scripts that rely
      # on stdenv being in PATH
      extra-deps = with pkgs; [ coreutils-full gnugrep gnused ];
      x11 = true;
      etcs = [ "localtime" "pulse" "passwd" ];
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
  zoom = lib.pipe {
    name = "zoom";
    devs = [ "dri" ];
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    camera = true;
    x11 = true;
    system-bus-socket = true;
    etcs = [ "pulse" "localtime" "resolv.conf" ];
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
  } [ withFonts withOpengl ];
  skypeforlinux = withFonts {
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
    etcs =
      [ "pulse" "localtime" "resolv.conf" "ssl/certs/ca-certificates.crt" ];
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
      deadbeef = super.deadbeef.override { wavpackSupport = true; };
      deadbeef-with-plugins = super.deadbeef-with-plugins.override {
        plugins = [
          super.deadbeef-mpris2-plugin
          (pkgs.callPackage ./deadbeef-statusnotifier-plugin.nix { })
        ];
      };
      zip-natspec = super.zip.override { enableNLS = true; };
      unzip-natspec = super.unzip.override { enableNLS = true; };
      wine-staging-full =
        super.wineWowPackages.full.override { wineRelease = "staging"; };
      firefox = super.firefox.override {
        extraNativeMessagingHosts = [ super.passff-host ];
      };
    })
    (self: super: {
      deadbeef-sandboxed = pkgs.symlinkJoin {
        name = "deadbeef";
        paths = [
          (sandbox super.deadbeef-with-plugins deadbeef)
          (sandbox super.deadbeef-with-plugins (withNet deadbeef))
          super.deadbeef-with-plugins
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
      firefox-sandboxed = pkgs.symlinkJoin {
        name = "firefox";
        paths = [ (sandbox super.firefox firefox) super.firefox ];
      };
      chromium-sandboxed = sandbox super.ungoogled-chromium chromium;
      pidgin-sandboxed = sandbox super.pidgin-with-plugins pidgin;
      qtox-sandboxed = sandbox super.qtox qtox;
      toxic-sandboxed = sandbox super.toxic toxic;
      tdesktop-sandboxed = sandbox super.tdesktop tdesktop;
      element-desktop-sandboxed = sandbox super.element-desktop element-desktop;
      qbittorrent-sandboxed = sandbox super.qbittorrent qbittorrent;
      feh-sandboxed = sandbox super.feh (viewer "feh");
      imv-sandboxed = sandbox super.imv (withOpengl (viewer "imv" // {
        devs = [ "dri" ];
        syses = [
          # Necessary for hardware acceleration
          "dev"
          "devices"
        ];
      }));
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
