{ config, lib, pkgs, ... }:

let
  flatpakFileAccess = pkgs.writeShellScriptBin "flatpak" ''
    [[ "$1" == "info" ]] || exit 1
    echo "$3 attempts to access $2" >> /tmp/.document-portal-log
    case "$3" in
      com.sandbox.firefox|com.sandbox.chromium|com.sandbox.tor-browser)
        case "''${2#--file-access=}" in
          $HOME/Downloads*) echo read-write;;
          *) echo read-only;;
        esac;;
      *)
        echo hidden;;
    esac
  '';
  sandbox = pkgs.callPackage ./sandbox/bwrap.nix { };
  wrap = drv: bins:
    let
      withOverrides = drv':
        drv' // {
          override = x: wrap (drv.override x) bins;
          overrideAttrs = x: wrap (drv.overrideAttrs x) bins;
          overrideDerivation = x: wrap (drv.overrideDerivation x) bins;
        };
    in withOverrides (lib.addMetaAttrs {
      sandboxed-bins = map (bin: bin.target-name or bin.name) bins;
      priority = -5;
    } (pkgs.symlinkJoin {
      name = drv.name + "-sandboxed";
      paths = map (sandbox drv) bins ++ [ drv ];
      postBuild = ''
        cd ${drv}
        grep -RlP "\\Q${
          lib.concatMapStringsSep "\\E|\\Q" (bin: "${drv}/bin/${bin.name}") bins
        }\\E" | while read file; do
          # do not replace wrapped binaries
          if ${
            lib.concatMapStringsSep " || "
            (bin: ''[[ "$file" == "bin/${bin.name}" ]]'') bins
          }; then
            continue
          fi
          rm -f "$out/$file"
          substitute "${drv}/$file" "$out/$file" ${
            lib.concatMapStringsSep " "
            (bin: "--replace-quiet ${drv}/bin/${bin.name} $out/bin/${bin.name}")
            bins
          }
          chmod --reference="${drv}/$file" "$out/$file"
        done || true
      '';
    }));
  withFonts = attrs:
    attrs // {
      extra-deps = (attrs.extra-deps or [ ])
        ++ config.fonts.fontconfig.confPackages;
      etcs = (attrs.etcs or [ ]) ++ [ "fonts" ];
      whitelist = if builtins.elem "~/" attrs.whitelist or [ ] then
        attrs.whitelist
      else
        (attrs.whitelist or [ ]) ++ [ "~/.cache/fontconfig" ];
    };
  withOpengl = attrs:
    attrs // {
      extra-deps = with config.hardware.graphics;
        (attrs.extra-deps or [ ]) ++ [ package ] ++ extraPackages;
      opengl = true;
    };
  withOpengl32 = attrs:
    attrs // {
      extra-deps = with config.hardware.graphics;
        (attrs.extra-deps or [ ]) ++ [ package32 ] ++ extraPackages32;
      opengl32 = true;
    };
  withHomeManager = name: attrs:
    attrs // {
      runtime-deps = [ "/etc/sandbox/common" "/etc/sandbox/${name}" ];
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
    # some apps might want to unpack archives in /tmp
    shared-tmp = true;
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
in {
  nixpkgs.overlays = [
    (self: _super: {
      sandboxed = {
        p7zip = wrap self.p7zip (map archiver-cfg [ "7z" "7za" "7zr" ]);
        _7zz = wrap self._7zz [ (archiver-cfg "7zz") ];
        unrar = wrap self.unrar [ (archiver-cfg "unrar") ];
        zip = wrap self.zip [ (archiver-cfg "zip") ];
        unzip = wrap self.unzip [ (archiver-cfg "unzip") ];
        jq = wrap self.jq [{
          name = "jq";
          unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
          shared-tmp = true;
          media = true;
          whitelist = [ "~/" ];
          blacklist = [ "~/.gnupg/" "~/.ssh/" ];
        }];
        libxml2 = wrap self.libxml2 [{
          name = "xmllint";
          unsetenvs = [ "DBUS_SESSION_BUS_ADDRESS" "MAIL" "SHELL" ];
          shared-tmp = true;
          media = true;
          whitelist = [ "~/" ];
          blacklist = [ "~/.gnupg/" "~/.ssh/" ];
        }];
        deadbeef-with-plugins = wrap self.deadbeef-with-plugins [
          (withFonts {
            name = "deadbeef";
            extra-deps = with pkgs; [
              gnome-themes-extra
              adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
              kdePackages.breeze
            ];
            pams = [ "pulse" "pipewire-0" ];
            etcs = [ "pulse" ];
            graphics = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            ro-media = true;
            dbus = [
              "talk=org.kde.StatusNotifierWatcher"
              "own=org.kde.StatusNotifierItem-2-1"
              "own=music.deadbeef.player"
              "own=org.mpris.MediaPlayer2.DeaDBeeF"
              "talk=org.freedesktop.Notifications"
              "talk=org.gtk.Settings"
              "talk=org.freedesktop.portal.Documents"
              "talk=org.freedesktop.portal.Desktop"
            ];
            ro-whitelist = [ "~/" ];
            whitelist = [ "~/.config/pulse/" "~/.config/deadbeef/" ];
            blacklist = [ "~/.gnupg/" "~/.ssh/" ];
          })
        ];
        mpv = wrap self.mpv [
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
            pams = [ "pulse" "pipewire-0" ];
            etcs = [ "pulse" ];
            # xdg-screensaver creates a lockfile in /tmp
            shared-tmp = true;
            unsetenvs = [ "MAIL" ];
            setenvs = [{
              name = "SHELL";
              value = "/run/current-system/sw/bin/bash";
            }];
            ro-media = true;
            dbus = [
              "own='org.mpris.MediaPlayer2.mpv.*'"
              "talk=org.freedesktop.portal.Desktop"
            ];
            ro-whitelist = [ "~/" ];
            whitelist = [ "~/.cache/fontconfig/" "~/.config/pulse/" ];
            blacklist = [ "~/.gnupg/" "~/.ssh/" ];
          } [ withFonts withOpengl (withHomeManager "mpv") ])
        ];
        vlc = wrap self.vlc [
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
            pams = [ "pulse" "pipewire-0" ];
            etcs = [ "pulse" ];
            unsetenvs = [ "MAIL" ];
            setenvs = [{
              name = "SHELL";
              value = "/run/current-system/sw/bin/bash";
            }];
            ro-media = true;
            dbus = [
              "talk=org.freedesktop.ScreenSaver"
              "talk=org.freedesktop.secrets"
              "talk=org.kde.kwalletd5"
              "talk=org.kde.StatusNotifierWatcher"
              "talk=org.mpris.MediaPlayer2.Player"
              "own=org.mpris.MediaPlayer2.vlc"
              "talk=org.freedesktop.Notifications"
              "talk=org.a11y.Bus"
            ];
            ro-whitelist = [ "~/" ];
            whitelist = [
              "~/.local/share/vlc/"
              "~/.cache/fontconfig/"
              "~/.config/pulse/"
            ];
            blacklist = [ "~/.gnupg/" "~/.ssh/" ];
          } [ withFonts withOpengl ])
        ];
        firefox = wrap self.firefox [
          (lib.pipe {
            name = "firefox";
            extra-deps = with pkgs; [
              gnome-themes-extra
              adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
              kde-gtk-config
              kdePackages.breeze
              config.i18n.glibcLocales
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
            pams = [ "gnupg" "pulse" "pipewire-0" ];
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "MAIL" ];
            setenvs = [{
              name = "SHELL";
              value = "/run/current-system/sw/bin/bash";
            }];
            unshare-net = false;
            system-dbus = [
              "talk=org.freedesktop.NetworkManager"
              "talk=org.freedesktop.login1"
            ];
            dbus = [
              "talk=org.freedesktop.portal.Documents"
              "talk=org.freedesktop.portal.Desktop"
              "talk=org.freedesktop.FileManager1"
              "talk=org.a11y.Bus"
              "talk=org.kde.kdeconnect"
              "talk=org.kde.KWin"
              "talk=org.gtk.Settings"
              "talk=org.freedesktop.PowerManagement"
              "talk=org.freedesktop.ScreenSaver"
              "own=org.kde.plasma.browser_integration"
              "own=org.mpris.MediaPlayer2.plasma-browser-integration"
              "own='org.mpris.MediaPlayer2.firefox.*'"
              "own='org.mozilla.firefox.*'"
            ];
            ro-whitelist = [
              "~/.password-store/"
              "~/.config/gtk-3.0/"
              "~/.config/kdeglobals"
              # if firefox finds /.flatpak-info it reads configs from this hardcoded path
              {
                from = "${self.firefox}/lib/firefox/mozilla.cfg";
                to = "/app/etc/firefox/mozilla.cfg";
              }
              {
                from = "${self.firefox}/lib/firefox/defaults/pref";
                to = "/app/etc/firefox/defaults/pref";
              }
            ];
            whitelist = [
              "~/.mozilla/"
              "~/.cache/mozilla/firefox/"
              "~/Downloads/"
              "~/.cache/fontconfig/"
              "~/.config/pulse/"
              "~/.gnupg/"
            ];
            flatpak = true;
          } [ withFonts withOpengl (withHomeManager "firefox") ])
        ];
        ungoogled-chromium = wrap self.ungoogled-chromium [
          (lib.pipe {
            name = "chromium";
            extra-deps = with pkgs; [
              gnome-themes-extra
              adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
              kde-gtk-config
              kdePackages.breeze
            ];
            devs = [ "dri" ];
            camera = true;
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
            ];
            graphics = true;
            pams = [ "gnupg" "pulse" "pipewire-0" ];
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            disable-userns = false;
            system-dbus = [
              "talk=org.bluez"
              "talk=org.freedesktop.Avahi"
              "talk=org.freedesktop.UPower"
            ];
            dbus = [
              "talk=org.freedesktop.portal.Documents"
              "talk=org.freedesktop.portal.Desktop"
              "talk=org.gtk.Settings"
              "talk=org.freedesktop.FileManager1"
              "talk=org.freedesktop.Notifications"
              "talk=org.freedesktop.ScreenSaver"
              "talk=org.freedesktop.secrets"
              "talk=org.kde.kwalletd5"
              "talk=org.kde.kwalletd6"
              "talk=org.freedesktop.PowerManagement"
              "own='org.mpris.MediaPlayer2.chromium.*'"
            ];
            ro-whitelist = [ "~/.config/gtk-3.0/" "~/.config/kdeglobals" ];
            whitelist = [
              "~/.config/chromium/"
              "~/.cache/chromium/"
              "~/Downloads/"
              "~/.cache/fontconfig/"
              "~/.config/pulse/"
            ];
            flatpak = true;
          } [ withFonts withOpengl ])
        ];
        qtox = wrap self.qtox [
          (lib.pipe {
            name = "qtox";
            extra-deps = with pkgs; [
              gnome-themes-extra
              adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
              kdePackages.breeze
            ];
            devs = [ "dri" ];
            camera = true;
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
            ];
            graphics = true;
            pams = [ "pulse" "pipewire-0" ];
            etcs = [ "pulse" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            dbus = [
              "talk=org.kde.StatusNotifierWatcher"
              "talk=org.freedesktop.Notifications"
              "talk=org.a11y.Bus"
              "talk=org.kde.KWin"
            ];
            ro-whitelist = [ "~/.config/kdeglobals" ];
            whitelist = [ "~/.config/tox/" "~/.cache/Tox/" "~/.config/pulse/" ];
          } [ withFonts withOpengl ])
        ];
        toxic = wrap self.toxic [
          (withHomeManager "toxic" {
            name = "toxic";
            extra-deps = with pkgs; [ config.i18n.glibcLocales ];
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
        gajim = wrap self.gajim [
          (withFonts {
            name = "gajim";
            extra-deps = with pkgs; [
              gnome-themes-extra
              adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
              kdePackages.breeze
            ];
            graphics = true;
            pams = [ "pulse" "pipewire-0" ];
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            system-dbus = [
              "talk=org.freedesktop.login1"
              "talk=org.freedesktop.NetworkManager"
            ];
            dbus = [
              "talk=org.freedesktop.FileManager1"
              "talk=org.gtk.Settings"
              "talk=org.freedesktop.portal.Desktop"
              "talk='org.mpris.MediaPlayer2.*'"
              "talk=org.freedesktop.ScreenSaver"
              "talk=org.freedesktop.secrets"
              "talk=org.kde.kwalletd5"
              "talk=org.freedesktop.Notifications"
              "talk=org.kde.StatusNotifierWatcher"
              "own=org.gajim.Gajim"
            ];
            ro-whitelist = [ "~/.config/gtk-3.0/" ];
            whitelist = [
              "~/.config/gajim/"
              "~/.cache/gajim/"
              "~/.local/share/gajim/"
              "~/.config/pulse/"
            ];
          })
        ];
        telegram-desktop = wrap self.telegram-desktop [
          (lib.pipe {
            name = "telegram-desktop";
            extra-deps = with pkgs; [
              gnome-themes-extra
              adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
              kdePackages.breeze
              config.i18n.glibcLocales
            ];
            devs = [ "dri" ];
            camera = true;
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
            ];
            graphics = true;
            pams = [ "pulse" "pipewire-0" ];
            etcs = [ "pulse" ];
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            dbus = [
              "own=org.telegram.desktop"
              "talk=org.kde.StatusNotifierWatcher"
              "talk=org.freedesktop.portal.Desktop"
              "talk=org.a11y.Bus"
              "talk=org.freedesktop.ScreenSaver"
              "talk=org.freedesktop.Notifications"
            ];
            ro-whitelist = [ "~/.config/kdeglobals" ];
            whitelist =
              [ "~/.local/share/TelegramDesktop/" "~/.config/pulse/" ];
          } [ withFonts withOpengl ])
        ];
        element-desktop = wrap self.element-desktop [
          (lib.pipe {
            name = "element-desktop";
            extra-deps = with pkgs; [
              gnome-themes-extra
              adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
              kdePackages.breeze
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
            pams = [ "pulse" "pipewire-0" ];
            etcs = [ "pulse" "ssl/certs/ca-certificates.crt" ];
            # Tray icon is stored in /tmp
            shared-tmp = true;
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            disable-userns = false;
            system-dbus = [ "talk=org.freedesktop.login1" ];
            dbus = [
              "talk=org.kde.StatusNotifierWatcher"
              "talk=org.freedesktop.ScreenSaver"
              "talk=org.freedesktop.secrets"
              "talk=org.gtk.Settings"
              "talk=org.freedesktop.portal.Desktop"
            ];
            ro-whitelist = [ "~/.config/gtk-3.0/" ];
            whitelist = [ "~/.config/Element/" "~/.config/pulse/" ];
          } [ withFonts withOpengl ])
        ];
        qbittorrent = wrap self.qbittorrent [
          (lib.pipe {
            name = "qbittorrent";
            extra-deps = with pkgs; [
              gnome-themes-extra
              adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
              kdePackages.breeze
              config.i18n.glibcLocales
            ];
            devs = [ "dri" ];
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
            ];
            graphics = true;
            # Creates lockfile there to launch a single instance
            shared-tmp = true;
            localtime = true;
            resolv-conf = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            system-dbus =
              [ "talk=org.freedesktop.login1" "talk=org.freedesktop.UPower" ];
            dbus = [
              "talk=org.freedesktop.portal.Desktop"
              "talk=org.kde.StatusNotifierWatcher"
              "talk=org.freedesktop.Notifications"
              "talk=org.freedesktop.PowerManagement"
              "talk=org.a11y.Bus"
            ];
            ro-whitelist = [ "~/.config/kdeglobals" ];
            whitelist = [
              "~/.local/share/qBittorrent/"
              "~/.config/qBittorrent/"
              "~/.cache/qBittorrent/"
              "~/Downloads/"
              "~/Torrents/"
            ];
          } [ withFonts withOpengl ])
        ];
        feh = wrap self.feh [ (withHomeManager "feh" (viewer-cfg "feh")) ];
        imv = wrap self.imv [
          (withOpengl (viewer-cfg "imv" // {
            devs = [ "dri" ];
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
            ];
          }))
        ];
        zathura = wrap self.zathura [
          (let cfg = viewer-cfg "zathura";
          in cfg // {
            extra-deps = with pkgs;
              cfg.extra-deps ++ [ config.i18n.glibcLocales ];
            whitelist = cfg.whitelist
              ++ [ "~/.local/share/zathura/" "~/Print/" ];
          })
        ];
        kdePackages.okular = wrap self.kdePackages.okular [
          (let cfg = withOpengl (viewer-cfg "okular");
          in cfg // {
            extra-deps = with pkgs;
              cfg.extra-deps ++ [
                gnome-themes-extra
                adwaita-icon-theme
                hicolor-icon-theme
                plasma-integration
                kdePackages.breeze
                config.i18n.glibcLocales
              ];
            devs = [ "dri" ];
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
            ];
            dbus = [
              "talk=org.kde.ActivityManager"
              "talk=org.kde.kdeconnect"
              "talk=org.a11y.Bus"
              "talk=org.freedesktop.portal.Desktop"
              "own=org.kde.okular-2"
            ];
            whitelist = cfg.whitelist ++ [
              "~/.local/share/okular/"
              # It stores lockfiles there: `okularpartrc.lock` and `okularrc.lock`
              "~/.config/"
              "~/Print/"
            ];
          })
        ];
        ffmpeg-full = wrap self.ffmpeg-full [
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
        wineWowPackages.stagingFull = wrap self.wineWowPackages.stagingFull [
          (lib.pipe {
            name = "wine";
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
            pams = [ "pulse" "pipewire-0" ];
            localtime = true;
            unsetenvs = [ "MAIL" ];
            setenvs = [{
              name = "SHELL";
              value = "/run/current-system/sw/bin/bash";
            }];
            unshare-cgroup = false;
            unshare-pid = false;
            seccomp = [ ];
            system-dbus = [
              "talk=org.freedesktop.UDisks2"
              "talk=org.freedesktop.NetworkManager"
            ];
            whitelist = [
              "\${WINEPREFIX:-~/.wine/}"
              "~/.cache/wine/"
              "~/.cache/winetricks/"
              "~/.config/pulse/"
            ];
          } [ withFonts withOpengl withOpengl32 ])
        ];
        libreoffice-fresh = wrap self.libreoffice-fresh (map (name:
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
            pams = [ "pulse" "pipewire-0" ];
            unsetenvs = [ "MAIL" "SHELL" ];
            # it finds launched instances through /tmp
            # in case it can't find them it asks to recover documents
            shared-tmp = true;
            media = true;
            dbus = [
              "talk='org.gtk.vfs.*'"
              "talk=org.freedesktop.portal.Desktop"
              "own=org.libreoffice.LibreOfficeIpc0"
              "talk=org.gtk.Settings"
              "talk=org.freedesktop.portal.Documents"
            ];
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
        wesnoth = wrap self.wesnoth [
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
        tor-browser-bundle-bin = wrap self.tor-browser-bundle-bin [
          (withOpengl {
            name = "tor-browser";
            extra-deps = with pkgs; [
              gnome-themes-extra
              adwaita-icon-theme
              hicolor-icon-theme
              plasma-integration
              kde-gtk-config
              kdePackages.breeze
              config.i18n.glibcLocales
            ];
            devs = [ "dri" ];
            syses = [
              # Necessary for hardware acceleration
              "dev"
              "devices"
              "bus"
            ];
            pams = [ "gnupg" "pulse" "pipewire-0" ];
            graphics = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            dbus = [
              "talk=org.freedesktop.portal.Documents"
              "talk=org.freedesktop.portal.Desktop"
              "talk=org.freedesktop.FileManager1"
              "talk=org.a11y.Bus"
              "talk=org.gtk.Settings"
              "talk=org.freedesktop.PowerManagement"
              "talk=org.freedesktop.ScreenSaver"
              "own='org.mpris.MediaPlayer2.firefox.*'"
              "own='org.mozilla.firefox.*'"
            ];
            ro-whitelist = [
              "~/.config/gtk-3.0/"
              "~/.config/kdeglobals"
              # if firefox finds /.flatpak-info it reads configs from this hardcoded path
              {
                from =
                  "${self.tor-browser-bundle-bin}/share/tor-browser/mozilla.cfg";
                to = "/app/etc/firefox/mozilla.cfg";
              }
              {
                from =
                  "${self.tor-browser-bundle-bin}/share/tor-browser/defaults/pref";
                to = "/app/etc/firefox/defaults/pref";
              }
            ];
            whitelist = [ "~/.tor\\ project/" "~/Downloads/" ];
            flatpak = true;
          })
        ];
        isync = wrap self.isync [{
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
        mu = wrap self.mu [{
          name = "mu";
          unsetenvs = [ "MAIL" "SHELL" ];
          whitelist = [ "~/Maildir/" "~/.cache/mu/" ];
        }];
        vdirsyncer = wrap self.vdirsyncer [
          (withHomeManager "vdirsyncer" {
            name = "vdirsyncer";
            bin-sh = true;
            extra-deps = with pkgs; [ coreutils-full pass gnupg ];
            pams = [ "gnupg" ];
            etcs = [ "ssl/certs/ca-certificates.crt" ];
            resolv-conf = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            ro-whitelist = [ "~/.password-store/" "~/.config/vdirsyncer/" ];
            whitelist =
              [ "~/Calendar/" "~/.local/share/vdirsyncer/" "~/.gnupg/" ];
          })
        ];
        claws-mail = wrap self.claws-mail [
          (withFonts {
            name = "claws-mail";
            extra-deps = with pkgs; [
              gnome-themes-extra
              adwaita-icon-theme
              hicolor-icon-theme
              kdePackages.breeze
            ];
            resolv-conf = true;
            graphics = true;
            unsetenvs = [ "MAIL" "SHELL" ];
            unshare-net = false;
            ro-whitelist = [ "~/.config/gtk-3.0/" ];
            whitelist = [ "~/.claws-mail/" ];
          })
        ];
        mc = lib.setPrio (-5) (self.mc.override {
          zip = self.sandboxed.zip.override { enableNLS = true; };
          unzip = self.sandboxed.unzip.override { enableNLS = true; };
        });
      };
    })
  ];

  environment = {
    systemPackages = [ flatpakFileAccess ];
    etc = let
      home-files = lib.mapAttrsToList (_name: value: value.home-files)
        config.home-manager.users;
      home-paths = lib.mapAttrsToList (_name: value: value.home.path)
        config.home-manager.users;
      home-deps-drv = paths:
        let
          drv =
            pkgs.runCommand "home-files" { disallowedReferences = home-files; }
            (lib.concatMapStrings (files:
              lib.concatMapStrings (path: ''
                [ -d ${files}/${path} ] && find ${files}/${path} -type l | xargs -r readlink -f >> $out
              '') paths) home-files);
        in pkgs.closureInfo { rootPaths = [ drv ]; };
    in {
      "sandbox/common".text = ''
        ${lib.concatStringsSep "\n" home-files}
        ${lib.concatStringsSep "\n" home-paths}
      '';
      "sandbox/mpv".source = "${home-deps-drv [ ".config/mpv" ]}/store-paths";
      "sandbox/firefox".source = "${home-deps-drv [ ".mozilla" ]}/store-paths";
      "sandbox/toxic".source = "${home-deps-drv [ ".config/tox" ]}/store-paths";
      "sandbox/feh".source = "${home-deps-drv [ ".config/feh" ]}/store-paths";
      "sandbox/vdirsyncer".source =
        "${home-deps-drv [ ".config/vdirsyncer" ]}/store-paths";
    };
  };

  system.activationScripts.installInitScript = let
    collectBins = attrs:
      if lib.isDerivation attrs then
        attrs.meta.sandboxed-bins or [ ]
      else
        map collectBins (lib.attrValues attrs);
  in lib.mkForce ''
    RED='\033[0;31m'
    NC='\033[0m'
    declare -A bins
    bins=(['${
      lib.concatStringsSep "']=1 ['" (lib.flatten (collectBins pkgs.sandboxed))
    }"']=1)
    find $systemConfig/sw/bin $systemConfig/etc/profiles/per-user/*/bin -print0 |
      while IFS= read -r -d ''' bin; do
        if [[ -v bins["$(basename "$bin")"] ]]; then
          grep ${pkgs.bubblewrap}/bin/bwrap "$bin" > /dev/null || echo -e "''${RED}$bin is not wrapped!''${NC}"
        fi
      done
  '';
}
