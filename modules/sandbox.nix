{ config, lib, pkgs, ... }:

let
  sandbox = pkgs.callPackage ./sandbox-bwrap.nix { };
  pid-hack = drv: name:
    pkgs.symlinkJoin {
      inherit name;
      paths = [ (pkgs.callPackage ./sandbox-pid-hack.nix { } drv name) drv ];
    };
  wrap = drv: bins:
    pkgs.symlinkJoin {
      name = drv.name + "-sandboxed";
      paths = (map (sandbox drv) bins) ++ [ drv ];
    };
  withFonts = attrs:
    attrs // {
      extra-deps = (attrs.extra-deps or [ ])
        ++ config.fonts.fontconfig.confPackages;
      etcs = (attrs.etcs or [ ]) ++ [ "fonts" ];
    };
  withOpengl = attrs:
    attrs // {
      extra-deps = with config.hardware.opengl;
        (attrs.extra-deps or [ ]) ++ [ package ] ++ extraPackages;
      opengl = true;
    };
  withOpengl32 = attrs:
    attrs // {
      extra-deps = with config.hardware.opengl;
        (attrs.extra-deps or [ ]) ++ [ package32 ] ++ extraPackages32;
      opengl32 = true;
    };
  withHomeManager = attrs:
    attrs // {
      extra-deps-no-transitive = (attrs.extra-deps-no-transitive or [ ])
        ++ lib.unique (lib.mapAttrsToList (name: value: value.home.path)
          config.home-manager.users);
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
      unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
      ro-whitelist = [ "~/" ];
      blacklist = [ "~/.gnupg/" "~/.ssh/" ];
    };
  deadbeef = withFonts {
    name = "deadbeef";
    extra-deps = with pkgs; [
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
      plasma-integration
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
    etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
    localtime = true;
    resolv-conf = true;
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" ];
    setenvs = [{
      name = "SHELL";
      value = "/run/current-system/sw/bin/bash";
    }];
    unshare-net = false;
    ro-whitelist = [ "~/.password-store/" "~/.config/gtk-3.0/" ];
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
    system-bus-socket = true;
    pams = [ "bus" "gnupg" "pulse" ];
    etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
    localtime = true;
    resolv-conf = true;
    unsetenvs = [ "MAIL" "SHELL" ];
    unshare-net = false;
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
      plasma-integration
    ];
    x11 = true;
    etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
    localtime = true;
    resolv-conf = true;
    pams = [ "bus" "pulse" ];
    unshare-net = false;
    unsetenvs = [ "MAIL" "SHELL" ];
    ro-whitelist = [ "~/.gtkrc-2.0" ];
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
      plasma-integration
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
    pams = [ "bus" "pulse" ];
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
  } [ withFonts withOpengl withHomeManager ];
  vlc = lib.pipe {
    name = "vlc";
    extra-deps = with pkgs; [ plasma-integration ];
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
  qtox = lib.pipe {
    name = "qtox";
    extra-deps = with pkgs; [
      qt5ct
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
      plasma-integration
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
    etcs = [ "pulse" ];
    localtime = true;
    resolv-conf = true;
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/qt5ct/" "~/.config/kdeglobals" ];
    whitelist = [ "~/.config/tox/" "~/.cache/Tox/" "~/.config/pulse/" ];
  } [ withFonts withOpengl ];
  toxic = {
    name = "toxic";
    extra-deps = with pkgs; [ glibcLocales ];
    devs = [ "dri" ];
    camera = true;
    x11 = true;
    pams = [ "pulse" ];
    etcs = [ "pulse" ];
    localtime = true;
    resolv-conf = true;
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    whitelist = [ "~/.config/tox/" "~/.config/pulse/" ];
  };
  tdesktop = lib.pipe {
    name = "telegram-desktop";
    extra-deps = with pkgs; [
      qt5ct
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
      plasma-integration
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
    etcs = [ "pulse" ];
    localtime = true;
    resolv-conf = true;
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/qt5ct/" "~/.config/kdeglobals" ];
    whitelist = [ "~/.local/share/TelegramDesktop/" "~/.config/pulse/" ];
  } [ withFonts withOpengl ];
  element-desktop = withFonts {
    name = "element-desktop";
    extra-deps = with pkgs; [
      qt5ct
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
      plasma-integration
    ];
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
    pams = [ "bus" "pulse" ];
    etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
    # Tray icon is stored in /tmp
    shared-tmp = true;
    localtime = true;
    resolv-conf = true;
    unsetenvs = [ "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/gtk-3.0/" ];
    whitelist = [ "~/.config/Element/" "~/.config/pulse/" ];
  };
  qbittorrent = lib.pipe {
    name = "qbittorrent";
    extra-deps = with pkgs; [
      qt5ct
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      hicolor-icon-theme
      plasma-integration
    ];
    devs = [ "dri" ];
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    x11 = true;
    pams = [ "bus" ];
    localtime = true;
    resolv-conf = true;
    unsetenvs = [ "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/qt5ct/" "~/.config/kdeglobals" ];
    whitelist = [
      "~/.local/share/data/qBittorrent/"
      "~/.config/qBittorrent/"
      "~/.cache/qBittorrent/"
      "~/Torrents/"
      "~/movies/"
    ];
  } [ withFonts withOpengl withHomeManager ];
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
      devs = [ "dri" "snd" ];
      syses = [
        # Necessary for hardware acceleration
        "dev"
        "devices"
      ];
      x11 = true;
      etcs = [ "ssl/certs/ca-certificates.crt" ];
      pams = [ "bus" "pulse" ];
      localtime = true;
      unsetenvs = [ "MAIL" ];
      setenvs = [{
        name = "SHELL";
        value = "/run/current-system/sw/bin/bash";
      }];
      unshare-cgroup = false;
      unshare-pid = false;
      seccomp = false;
      whitelist = [
        "\${WINEPREFIX:-~/.wine/}"
        "~/.cache/wine/"
        "~/.cache/winetricks/"
        "~/.config/pulse/"
      ];
    } [ withFonts withOpengl withOpengl32 ];
  libreoffice = name:
    withFonts {
      inherit name;
      # coreutils-full, gnugrep, gnused are needed because it's
      # system default stdenv and libreoffice has scripts that rely
      # on stdenv being in PATH
      extra-deps = with pkgs; [
        coreutils-full
        gnugrep
        gnused
        plasma-integration
      ];
      x11 = true;
      etcs = [ "pulse" "passwd" ];
      localtime = true;
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
    etcs = [ "pulse" ];
    localtime = true;
    resolv-conf = true;
    pams = [ "bus" "pulse" ];
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" ];
    setenvs = [{
      name = "SHELL";
      value = "/run/current-system/sw/bin/bash";
    }];
    unshare-net = false;
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
    etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
    localtime = true;
    resolv-conf = true;
    unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
    unshare-net = false;
    ro-whitelist = [ "~/.config/gtk-3.0/" ];
    whitelist = [ "~/.config/skypeforlinux/" "~/.config/pulse/" ];
  };
in {
  nixpkgs.overlays = [
    (self: super: {
      sandbox-seccomp = pkgs.callPackage ./sandbox-seccomp.nix { };
      xcb-client-id = pkgs.callPackage ./xcb-client-id.nix { };
    })
    (self: super: {
      deadbeef-sandboxed =
        wrap (pid-hack super.deadbeef-with-plugins "deadbeef") [ deadbeef ];
      p7zip-sandboxed = wrap super.p7zip (map archiver [ "7z" "7za" "7zr" ]);
      _7zz-sandboxed = wrap super._7zz [ (archiver "7zz") ];
      unrar-sandboxed = wrap super.unrar [ (archiver "unrar") ];
      zip-natspec-sandboxed = wrap super.zip-natspec [ (archiver "zip") ];
      unzip-natspec-sandboxed = wrap super.unzip-natspec [ (archiver "unzip") ];
      mpv-sandboxed = wrap super.mpv-with-scripts [ mpv ];
      vlc-sandboxed = wrap super.vlc [ vlc ];
      firefox-sandboxed = wrap super.firefox [ firefox ];
      firefox-wayland-sandboxed = wrap super.firefox-wayland [ firefox ];
      chromium-sandboxed = wrap super.ungoogled-chromium [ chromium ];
      chromium-wayland-sandboxed =
        wrap super.ungoogled-chromium-wayland [ chromium ];
      pidgin-sandboxed = wrap super.pidgin-with-plugins [ pidgin ];
      qtox-sandboxed = wrap (pid-hack super.qtox "qtox") [ qtox ];
      toxic-sandboxed = wrap super.toxic [ toxic ];
      tdesktop-sandboxed = wrap super.tdesktop [ tdesktop ];
      element-desktop-sandboxed =
        wrap super.element-desktop [ element-desktop ];
      element-desktop-wayland-sandboxed =
        wrap super.element-desktop-wayland [ element-desktop ];
      qbittorrent-sandboxed =
        wrap (pid-hack super.qbittorrent "qbittorrent") [ qbittorrent ];
      feh-sandboxed = wrap super.feh [ (viewer "feh") ];
      imv-sandboxed = wrap super.imv [
        (withOpengl (viewer "imv" // {
          devs = [ "dri" ];
          syses = [
            # Necessary for hardware acceleration
            "dev"
            "devices"
          ];
        }))
      ];
      zathura-sandboxed = wrap super.zathura [
        ((viewer "zathura") // {
          whitelist = [ "~/.local/share/zathura/" "~/Print/" ];
        })
      ];
      ffmpeg-full-sandboxed = wrap super.ffmpeg-full [ ffmpeg ffprobe ];
      wine-staging-full-sandboxed =
        wrap super.wineWowPackages.stagingFull (map wine [ "wine" "winecfg" ]);
      libreoffice-fresh-sandboxed = wrap super.libreoffice-fresh
        (map libreoffice [
          "libreoffice"
          "sbase"
          "scalc"
          "sdraw"
          "simpress"
          "smath"
          "soffice"
          "swriter"
          "unopkg"
        ]);
      wesnoth-sandboxed = wrap super.wesnoth [
        (withFonts {
          name = "wesnoth";
          pams = [ "pulse" ];
          etcs = [ "pulse" ];
          x11 = true;
          unsetenvs = [ "MAIL" "SHELL" ];
          unshare-net = false;
          whitelist = [
            "~/.config/wesnoth/"
            "~/.cache/wesnoth/"
            "~/.local/share/wesnoth/"
            "~/.config/pulse/"
          ];
        })
      ];
      tor-browser-bundle-bin-sandboxed =
        wrap super.tor-browser-bundle-bin [ tor-browser ];
      zoom-us-sandboxed = wrap super.zoom-us [ zoom ];
      skypeforlinux-sandboxed = wrap super.skypeforlinux [ skypeforlinux ];
    })
  ];
}
