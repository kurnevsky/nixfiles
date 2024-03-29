{ config, lib, pkgs, ... }:

let
  sandbox = pkgs.callPackage ./sandbox/bwrap.nix { };
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
    media = true;
    whitelist = [ "~/" ];
    blacklist = [ "~/.gnupg/" "~/.ssh/" ];
  };
  viewer-cfg = name:
    withFonts {
      inherit name;
      graphics = true;
      unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
      ro-media = true;
      ro-whitelist = [ "~/" ];
      blacklist = [ "~/.gnupg/" "~/.ssh/" ];
    };
  wrappers = [
    {
      predicate = lib.hasPrefix "deadbeef-";
      config = drv:
        wrap drv [
          (withFonts {
            name = "deadbeef";
            extra-deps = with pkgs; [
              gnome-themes-extra
              gnome.adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
            ];
            pams = [
              # Necessary for MPRIS2
              "bus"
              "pulse"
              "pipewire-0"
            ];
            etcs = [ "pulse" ];
            graphics = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            ro-media = true;
            ro-whitelist = [ "~/" ];
            whitelist = [ "~/.config/pulse/" "~/.config/deadbeef/" ];
            blacklist = [ "~/.gnupg/" "~/.ssh/" ];
          })
        ];
    }
    {
      predicate = lib.hasPrefix "p7zip-";
      config = drv: wrap drv (map archiver-cfg [ "7z" "7za" "7zr" ]);
    }
    {
      predicate = lib.hasPrefix "7zz-";
      config = drv: wrap drv [ (archiver-cfg "7zz") ];
    }
    {
      predicate = lib.hasPrefix "unrar-";
      config = drv: wrap drv [ (archiver-cfg "unrar") ];
    }
    {
      unused = true;
      predicate = lib.hasPrefix "zip-";
      config = drv: wrap drv [ (archiver-cfg "zip") ];
    }
    {
      predicate = lib.hasPrefix "unzip-";
      config = drv: wrap drv [ (archiver-cfg "unzip") ];
    }
    {
      predicate = lib.hasPrefix "jq-";
      config = drv:
        wrap drv [{
          name = "jq";
          unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
          shared-tmp = true;
          media = true;
          whitelist = [ "~/" ];
          blacklist = [ "~/.gnupg/" "~/.ssh/" ];
        }];
    }
    {
      predicate = lib.hasPrefix "libxml2-";
      config = drv:
        wrap drv [{
          name = "xmllint";
          unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
          shared-tmp = true;
          media = true;
          whitelist = [ "~/" ];
          blacklist = [ "~/.gnupg/" "~/.ssh/" ];
        }];
    }
    {
      predicate = lib.hasPrefix "mpv-";
      config = drv:
        wrap drv [
          (lib.pipe {
            name = "mpv";
            bin-sh = true;
            extra-deps = with pkgs; [ plasma-integration ];
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
            pams = [ "bus" "pulse" "pipewire-0" ];
            etcs = [ "pulse" ];
            # xdg-screensaver creates a lockfile in /tmp
            shared-tmp = true;
            unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" ];
            setenvs = [{
              name = "SHELL";
              value = "/run/current-system/sw/bin/bash";
            }];
            ro-media = true;
            ro-whitelist = [ "~/" ];
            whitelist = [ "~/.cache/fontconfig/" "~/.config/pulse/" ];
            blacklist = [ "~/.gnupg/" "~/.ssh/" ];
          } [ withFonts withOpengl (withHomeManager [ ".config/mpv" ]) ])
        ];
    }
    {
      predicate = lib.hasPrefix "vlc-";
      config = drv:
        wrap drv [
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
            pams = [ "bus" "pulse" "pipewire-0" ];
            etcs = [ "pulse" ];
            unsetenvs = [ "MAIL" ];
            setenvs = [{
              name = "SHELL";
              value = "/run/current-system/sw/bin/bash";
            }];
            ro-media = true;
            ro-whitelist = [ "~/" ];
            whitelist = [
              "~/.local/share/vlc/"
              "~/.cache/fontconfig/"
              "~/.config/pulse/"
            ];
            blacklist = [ "~/.gnupg/" "~/.ssh/" ];
          } [ withFonts withOpengl ])
        ];
    }
    {
      predicate = lib.hasPrefix "firefox-";
      config = drv:
        wrap drv [
          (lib.pipe {
            name = "firefox";
            extra-deps = with pkgs; [
              qt5ct
              gnome-themes-extra
              gnome.adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
              kde-gtk-config
            ];
            devs = [ "dri" ];
            camera = true;
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
              "bus"
            ];
            graphics = true;
            pams = [ "bus" "gnupg" "pulse" "pipewire-0" ];
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" ];
            setenvs = [{
              name = "SHELL";
              value = "/run/current-system/sw/bin/bash";
            }];
            unshare-net = false;
            ro-whitelist = [
              "~/.password-store/"
              "~/.config/gtk-3.0/"
              "~/.config/qt5ct/"
              "~/.config/kdeglobals"
            ];
            whitelist = [
              "~/.mozilla/"
              "~/.cache/mozilla/firefox/"
              "~/Downloads/"
              "~/.cache/fontconfig/"
              "~/.config/pulse/"
              "~/.gnupg/"
            ];
          } [ withFonts withOpengl (withHomeManager [ ".mozilla" ]) ])
        ];
    }
    {
      predicate = name:
        lib.hasPrefix "chromium-" name
        || lib.hasPrefix "ungoogled-chromium-" name;
      config = drv:
        wrap drv [
          (lib.pipe {
            name = "chromium";
            extra-deps = with pkgs; [
              qt5ct
              gnome-themes-extra
              gnome.adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
              kde-gtk-config
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
            pams = [ "bus" "gnupg" "pulse" "pipewire-0" ];
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            disable-userns = false;
            ro-whitelist = [
              "~/.config/gtk-3.0/"
              "~/.config/qt5ct/"
              "~/.config/kdeglobals"
            ];
            whitelist = [
              "~/.config/chromium/"
              "~/.cache/chromium/"
              "~/Downloads/"
              "~/.cache/fontconfig/"
              "~/.config/pulse/"
            ];
          } [ withFonts withOpengl ])
        ];
    }
    {
      predicate = lib.hasPrefix "qtox-";
      config = drv:
        wrap drv [
          (lib.pipe {
            name = "qtox";
            extra-deps = with pkgs; [
              qt5ct
              gnome-themes-extra
              gnome.adwaita-icon-theme
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
            pams = [ "bus" "pulse" "pipewire-0" ];
            etcs = [ "pulse" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
            unshare-net = false;
            ro-whitelist = [ "~/.config/qt5ct/" "~/.config/kdeglobals" ];
            whitelist = [ "~/.config/tox/" "~/.cache/Tox/" "~/.config/pulse/" ];
          } [ withFonts withOpengl ])
        ];
    }
    {
      predicate = lib.hasPrefix "toxic-";
      config = drv:
        wrap drv [
          (withHomeManager [ ".config/tox" ] {
            name = "toxic";
            extra-deps = with pkgs; [ glibcLocales ];
            devs = [ "dri" ];
            camera = true;
            graphics = true;
            pams = [ "pulse" "pipewire-0" ];
            etcs = [ "pulse" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
            unshare-net = false;
            whitelist = [ "~/.config/tox/" "~/.config/pulse/" ];
          })
        ];
    }
    {
      predicate = lib.hasPrefix "gajim-";
      config = drv:
        wrap drv [
          (withFonts {
            name = "gajim";
            extra-deps = with pkgs; [
              gnome-themes-extra
              gnome.adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
            ];
            graphics = true;
            pams = [
              # Necessary for kwallet
              "bus"
              "pulse"
              "pipewire-0"
            ];
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            ro-whitelist = [ "~/.config/gtk-3.0/" ];
            whitelist = [
              "~/.config/gajim/"
              "~/.cache/gajim/"
              "~/.local/share/gajim/"
              "~/.config/pulse/"
            ];
          })
        ];
    }
    {
      predicate = lib.hasPrefix "telegram-desktop-";
      config = drv:
        wrap drv [
          (lib.pipe {
            name = "telegram-desktop";
            extra-deps = with pkgs; [
              qt5ct
              gnome-themes-extra
              gnome.adwaita-icon-theme
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
            pams = [ "bus" "pulse" "pipewire-0" ];
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
    }
    {
      predicate = lib.hasPrefix "element-desktop-";
      config = drv:
        wrap drv [
          (lib.pipe {
            name = "element-desktop";
            extra-deps = with pkgs; [
              qt5ct
              gnome-themes-extra
              gnome.adwaita-icon-theme
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
            pams = [ "bus" "pulse" "pipewire-0" ];
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            # Tray icon is stored in /tmp
            shared-tmp = true;
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            disable-userns = false;
            ro-whitelist = [ "~/.config/gtk-3.0/" ];
            whitelist = [ "~/.config/Element/" "~/.config/pulse/" ];
          } [ withFonts withOpengl ])
        ];
    }
    {
      predicate = lib.hasPrefix "qbittorrent-";
      config = drv:
        wrap drv [
          (lib.pipe {
            name = "qbittorrent";
            extra-deps = with pkgs; [
              qt5ct
              gnome-themes-extra
              gnome.adwaita-icon-theme
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
              "~/Downloads/"
              "~/Torrents/"
            ];
          } [ withFonts withOpengl ])
        ];
    }
    {
      predicate = lib.hasPrefix "feh-";
      config = drv:
        wrap drv [ (withHomeManager [ ".config/feh" ] (viewer-cfg "feh")) ];
    }
    {
      predicate = lib.hasPrefix "imv-";
      config = drv:
        wrap drv [
          (withOpengl (viewer-cfg "imv" // {
            devs = [ "dri" ];
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
            ];
          }))
        ];
    }
    {
      predicate = lib.hasPrefix "zathura-";
      config = drv:
        wrap drv [
          ((viewer-cfg "zathura") // {
            whitelist = [ "~/.local/share/zathura/" "~/Print/" ];
          })
        ];
    }
    {
      predicate = lib.hasPrefix "ffmpeg-";
      config = drv:
        wrap drv [
          {
            name = "ffmpeg";
            devs = [ "dri" ];
            camera = true;
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
            ];
            media = true;
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
            ro-media = true;
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
    }
    {
      predicate = lib.hasPrefix "wine-";
      config = drv:
        wrap drv (map (name:
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
            pams = [ "bus" "pulse" "pipewire-0" ];
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
          } [ withFonts withOpengl withOpengl32 ]) [ "wine" "winecfg" ]);
    }
    {
      predicate = lib.hasPrefix "libreoffice-";
      config = drv:
        wrap drv (map (name:
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
            pams = [ "bus" "pulse" "pipewire-0" ];
            unsetenvs = [ "MAIL" "SHELL" ];
            media = true;
            whitelist = [ "~/" ];
            blacklist = [ "~/.gnupg/" "~/.ssh/" ];
          }) [
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
    }
    {
      predicate = lib.hasPrefix "wesnoth-";
      config = drv:
        wrap drv [
          (lib.pipe {
            name = "wesnoth";
            devs = [ "dri" ];
            syses = [ "dev" "devices" ];
            pams = [ "pulse" "pipewire-0" ];
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
          } [ withFonts withOpengl ])
        ];
    }
    {
      predicate = lib.hasPrefix "tor-browser-";
      config = drv:
        wrap drv [{
          name = "tor-browser";
          graphics = true;
          unsetenvs = [ "MAIL" "SHELL" ];
          unshare-net = false;
          whitelist = [ "~/.tor\\ project/" "~/Downloads/" ];
        }];
    }
    {
      predicate = name: name == "isync";
      config = drv:
        wrap drv [{
          name = "mbsync";
          bin-sh = true;
          extra-deps = with pkgs; [ coreutils-full cloud-mdir-sync pass gnupg ];
          pams = [ "gnupg" "cms.sock" ];
          etcs = [ "ssl/certs/ca-certificates.crt" ];
          resolv-conf = true;
          unsetenvs = [ "MAIL" "SHELL" ];
          unshare-net = false;
          ro-whitelist = [ "~/.password-store/" "~/.mbsyncrc" ];
          whitelist = [ "~/Maildir/" "~/.gnupg/" ];
        }];
    }
    {
      predicate = lib.hasPrefix "mu-";
      config = drv:
        wrap drv [{
          name = "mu";
          unsetenvs = [ "MAIL" "SHELL" ];
          whitelist = [ "~/Maildir/" "~/.cache/mu/" ];
        }];
    }
    {
      predicate = lib.hasPrefix "claws-mail-";
      config = drv:
        wrap drv [
          (withFonts {
            name = "claws-mail";
            extra-deps = with pkgs; [
              qt5ct
              gnome-themes-extra
              gnome.adwaita-icon-theme
              hicolor-icon-theme
            ];
            resolv-conf = true;
            graphics = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            ro-whitelist = [ "~/.config/gtk-3.0/" ];
            whitelist = [ "~/.claws-mail/" ];
          })
        ];
    }
    {
      unused = true;
      predicate = lib.hasPrefix "zoom-";
      config = drv:
        wrap drv [
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
            pams = [ "bus" "pulse" "pipewire-0" ];
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
    }
    {
      unused = true;
      predicate = lib.hasPrefix "skypeforlinux-";
      config = drv:
        wrap drv [
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
            pams = [ "pulse" "pipewire-0" ];
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
            unshare-net = false;
            ro-whitelist = [ "~/.config/gtk-3.0/" ];
            whitelist = [ "~/.config/skypeforlinux/" "~/.config/pulse/" ];
          })
        ];
    }
  ];
in {
  nixpkgs.overlays = [
    (self: super: {
      mc = super.mc.override {
        zip = wrap (super.zip.override { enableNLS = true; })
          [ (archiver-cfg "zip") ];
        unzip = wrap (super.unzip.override { enableNLS = true; })
          [ (archiver-cfg "unzip") ];
      };
    })
  ];

  environment = let
    env = pkgs.symlinkJoin {
      name = "sandboxed";
      paths = assert lib.all (wrapper:
        (wrapper.unused or false) || lib.any (drv: wrapper.predicate drv.name)
        config.environment.systemPackages) wrappers;
        lib.concatMap (drv:
          lib.concatMap (wrapper:
            if wrapper.predicate drv.name then
              [ (wrapper.config drv) ]
            else
              [ ]) wrappers) config.environment.systemPackages;
    };
  in {
    extraInit = ''
      export PATH="/etc/sandboxed:$PATH"
    '';
    etc.sandboxed.source = "${env}/bin";
  };
}
