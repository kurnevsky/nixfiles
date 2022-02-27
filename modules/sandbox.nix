{ config, lib, pkgs, ... }:

let
  sandbox = pkgs.callPackage ./sandbox-bwrap.nix { };
  pid-hack = drv: name:
    pkgs.symlinkJoin {
      inherit name;
      paths = [ (pkgs.callPackage ./sandbox-pid-hack.nix { } drv name) drv ];
    };
  wrap = drv: bins:
    # Don't join with original drv because only bins will be used
    if lib.length bins == 1 then
      sandbox drv (lib.head bins)
    else
      pkgs.symlinkJoin {
        name = drv.name + "-sandboxed";
        paths = map (sandbox drv) bins;
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
  withHomeManager = paths:
    let
      home-files = lib.mapAttrsToList (name: value: value.home-files)
        config.home-manager.users;
      home-paths = lib.mapAttrsToList (name: value: value.home.path)
        config.home-manager.users;
      home-deps-drv =
        pkgs.runCommand "home-files" { disallowedReferences = home-files; }
        (lib.concatMapStrings (files:
          lib.concatMapStrings (path: ''
            [ -d ${files}/${path} ] && find ${files}/${path} -type l | xargs -r readlink -f >> $out
          '') paths) home-files);
    in attrs:
    attrs // {
      extra-deps-no-transitive = (attrs.extra-deps-no-transitive or [ ])
        ++ home-files ++ home-paths;
      extra-deps = (attrs.extra-deps or [ ])
        ++ (if paths == [ ] then [ ] else [ home-deps-drv ]);
    };
  archiver-cfg = name: {
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
  viewer-cfg = name:
    withFonts {
      inherit name;
      graphics = true;
      unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
      ro-whitelist = [ "~/" ];
      blacklist = [ "~/.gnupg/" "~/.ssh/" ];
    };
  firefox-cfg = lib.pipe {
    name = "firefox";
    devs = [ "dri" ];
    camera = true;
    syses = [
      # Necessary for hardware acceleration
      "dev"
      "devices"
    ];
    graphics = true;
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
  } [ withFonts withOpengl (withHomeManager [ ".mozilla" ]) ];
  chromium-cfg = lib.pipe {
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
    graphics = true;
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
  element-desktop-cfg = withFonts {
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
    graphics = true;
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
  wine-cfg = name:
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
      graphics = true;
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
  libreoffice-cfg = name:
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
      graphics = true;
      etcs = [ "pulse" "passwd" ];
      localtime = true;
      pams = [ "bus" "pulse" ];
      unsetenvs = [ "MAIL" "SHELL" ];
      whitelist = [ "~/" ];
      blacklist = [ "~/.gnupg/" "~/.ssh/" ];
    };
in {
  nixpkgs.overlays = [
    (self: super: {
      sandbox-seccomp = pkgs.callPackage ./sandbox-seccomp.nix { };
      xcb-client-id = pkgs.callPackage ./xcb-client-id.nix { };
    })
    (self: super: {
      sandboxed = {
        deadbeef = wrap (pid-hack super.deadbeef-with-plugins "deadbeef") [
          (withFonts {
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
            graphics = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            ro-whitelist = [ "~/" ];
            whitelist = [ "~/.config/pulse/" "~/.config/deadbeef/" ];
            blacklist = [ "~/.gnupg/" "~/.ssh/" ];
          })
        ];
        p7zip = wrap super.p7zip (map archiver-cfg [ "7z" "7za" "7zr" ]);
        _7zz = wrap super._7zz [ (archiver-cfg "7zz") ];
        unrar = wrap super.unrar [ (archiver-cfg "unrar") ];
        zip-natspec = wrap super.zip-natspec [ (archiver-cfg "zip") ];
        unzip-natspec = wrap super.unzip-natspec [ (archiver-cfg "unzip") ];
        mpv = wrap super.mpv-with-scripts [
          (lib.pipe {
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
            graphics = true;
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
          } [ withFonts withOpengl (withHomeManager [ ".config/mpv" ]) ])
        ];
        vlc = wrap super.vlc [
          (lib.pipe {
            name = "vlc";
            extra-deps = with pkgs; [ plasma-integration ];
            devs = [ "dri" ];
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
            ];
            graphics = true;
            pams = [ "bus" "pulse" ];
            etcs = [ "pulse" ];
            unsetenvs = [ "MAIL" ];
            setenvs = [{
              name = "SHELL";
              value = "/run/current-system/sw/bin/bash";
            }];
            ro-whitelist = [ "~/" ];
            whitelist = [
              "~/.local/share/vlc/"
              "~/.cache/fontconfig/"
              "~/.config/pulse/"
            ];
            blacklist = [ "~/.gnupg/" "~/.ssh/" ];
          } [ withFonts withOpengl ])
        ];
        firefox = wrap super.firefox [ firefox-cfg ];
        firefox-wayland = wrap super.firefox-wayland [ firefox-cfg ];
        chromium = wrap super.ungoogled-chromium [ chromium-cfg ];
        chromium-wayland =
          wrap super.ungoogled-chromium-wayland [ chromium-cfg ];
        pidgin = wrap super.pidgin-with-plugins [
          (withFonts {
            name = "pidgin";
            extra-deps = with pkgs; [
              gnome-themes-extra
              gnome3.adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
            ];
            graphics = true;
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            localtime = true;
            resolv-conf = true;
            pams = [ "bus" "pulse" ];
            unshare-net = false;
            unsetenvs = [ "MAIL" "SHELL" ];
            ro-whitelist = [ "~/.gtkrc-2.0" ];
            whitelist = [ "~/.purple/" "~/.config/pulse/" ];
          })
        ];
        qtox = wrap (pid-hack super.qtox "qtox") [
          (lib.pipe {
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
            graphics = true;
            pams = [ "bus" "pulse" ];
            etcs = [ "pulse" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
            unshare-net = false;
            ro-whitelist = [ "~/.config/qt5ct/" "~/.config/kdeglobals" ];
            whitelist = [ "~/.config/tox/" "~/.cache/Tox/" "~/.config/pulse/" ];
          } [ withFonts withOpengl ])
        ];
        toxic = wrap super.toxic [
          (withHomeManager [ ".config/tox" ] {
            name = "toxic";
            extra-deps = with pkgs; [ glibcLocales ];
            devs = [ "dri" ];
            camera = true;
            graphics = true;
            pams = [ "pulse" ];
            etcs = [ "pulse" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
            unshare-net = false;
            whitelist = [ "~/.config/tox/" "~/.config/pulse/" ];
          })
        ];
        tdesktop = wrap super.tdesktop [
          (lib.pipe {
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
            graphics = true;
            pams = [ "bus" "pulse" ];
            etcs = [ "pulse" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
            unshare-net = false;
            ro-whitelist = [ "~/.config/qt5ct/" "~/.config/kdeglobals" ];
            whitelist =
              [ "~/.local/share/TelegramDesktop/" "~/.config/pulse/" ];
          } [ withFonts withOpengl ])
        ];
        element-desktop = wrap super.element-desktop [ element-desktop-cfg ];
        element-desktop-wayland =
          wrap super.element-desktop-wayland [ element-desktop-cfg ];
        qbittorrent = wrap (pid-hack super.qbittorrent "qbittorrent") [
          (lib.pipe {
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
            graphics = true;
            pams = [ "bus" ];
            # Creates lockfile there to launch a single instance
            shared-tmp = true;
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
          } [ withFonts withOpengl ])
        ];
        feh = wrap super.feh
          [ (withHomeManager [ ".config/feh" ] (viewer-cfg "feh")) ];
        imv = wrap super.imv [
          (withOpengl (viewer-cfg "imv" // {
            devs = [ "dri" ];
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
            ];
          }))
        ];
        zathura = wrap super.zathura [
          ((viewer-cfg "zathura") // {
            whitelist = [ "~/.local/share/zathura/" "~/Print/" ];
          })
        ];
        ffmpeg-full = wrap super.ffmpeg-full [
          {
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
          }
          {
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
          }
        ];
        wine-staging-full = wrap super.wineWowPackages.stagingFull
          (map wine-cfg [ "wine" "winecfg" ]);
        libreoffice-fresh = wrap super.libreoffice-fresh (map libreoffice-cfg [
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
        wesnoth = wrap super.wesnoth [
          (withFonts {
            name = "wesnoth";
            pams = [ "pulse" ];
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            localtime = true;
            resolv-conf = true;
            graphics = true;
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
        tor-browser-bundle-bin = wrap super.tor-browser-bundle-bin [{
          name = "tor-browser";
          graphics = true;
          unsetenvs = [ "MAIL" "SHELL" ];
          unshare-net = false;
          whitelist = [ "~/.local/share/tor-browser/" ];
        }];
        zoom-us = wrap super.zoom-us [
          (lib.pipe {
            name = "zoom";
            devs = [ "dri" ];
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
            ];
            camera = true;
            graphics = true;
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
          } [ withFonts withOpengl ])
        ];
        skypeforlinux = wrap super.skypeforlinux [
          (withFonts {
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
            graphics = true;
            pams = [ "pulse" ];
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
            unshare-net = false;
            ro-whitelist = [ "~/.config/gtk-3.0/" ];
            whitelist = [ "~/.config/skypeforlinux/" "~/.config/pulse/" ];
          })
        ];
      };
    })
    (self: super: {
      mc = super.mc.override {
        zip = super.sandboxed.zip-natspec;
        unzip = super.sandboxed.unzip-natspec;
      };
    })
  ];

  environment = let
    env = pkgs.buildEnv {
      name = "sandboxed";
      paths = with pkgs.sandboxed; [
        deadbeef
        p7zip
        _7zz
        unrar
        zip-natspec
        unzip-natspec
        mpv
        vlc
        # firefox
        firefox-wayland
        # chromium
        chromium-wayland
        pidgin
        qtox
        toxic
        tdesktop
        # element-desktop
        element-desktop-wayland
        qbittorrent
        feh
        imv
        zathura
        ffmpeg-full
        wine-staging-full
        libreoffice-fresh
        wesnoth
        tor-browser-bundle-bin
      ];
    };
  in {
    extraInit = ''
      export PATH="/etc/sandboxed:$PATH"
    '';
    etc.sandboxed.source = "${env}/bin";
  };
}
