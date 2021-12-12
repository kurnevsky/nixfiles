{ config, lib, pkgs, ... }:

{
  environment.etc."taffybar.css".source = ./taffybar/taffybar.css;

  security.wrappers.xscreensaver-auth = {
    owner = "root";
    group = "root";
    source = "${pkgs.xscreensaver}/libexec/xscreensaver/xscreensaver-auth";
  };

  programs.qt5ct.enable = true;

  services.xserver = {
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = pkgs: with pkgs; [ dbus regex-compat taffybar ];
      config = builtins.readFile ./xmonad/xmonad.hs;
      ghcArgs = [ "-O2" "${./xmonad/lib/XMonad/Util/Compton.hs}" ];
    };
    displayManager.startx.enable = true;
  };

  # system.replaceRuntimeDependencies can be used to make fast fixes
  nixpkgs.overlays = [
    (self: super: {
      xscreensaver = super.xscreensaver.overrideAttrs (attrs: {
        patches = (super.xscreensaver.patches or [ ])
          ++ [ ./xscreensaver-hack.diff ];
      });
    })
    (self: super: {
      haskellPackages = super.haskellPackages.override {
        overrides = haskellSelf: haskellSuper: {
          xmonad-contrib = haskellSuper.xmonad-contrib.overrideAttrs
            (oldAttrs: {
              patches = (oldAttrs.patches or [ ]) ++ [
                (pkgs.fetchpatch {
                  name = "ewmh-windows-ordering.patch";
                  url =
                    "https://github.com/kurnevsky/xmonad-contrib/commit/b6ab084f76c182dc2722e50933236358b92eb12a.patch";
                  sha256 =
                    "sha256-xXBhDkoqvl33m/IvCqt10jSouK2lo+AZxybh4ZNsSYk=";
                })
              ];
            });
          status-notifier-item = haskellSuper.status-notifier-item.overrideAttrs
            (oldAttrs: {
              src = pkgs.fetchFromGitHub {
                owner = "taffybar";
                repo = "status-notifier-item";
                rev = "c5d7d898e4f13ec9864e5047b6da25de62535672";
                sha256 = "sha256-EJHvVtYQvohhOhznY5Iy3GR0zyjwMF+lsCr5hgL3ziw=";
              };
              prePatch = "${pkgs.hpack}/bin/hpack";
            });
          gtk-sni-tray = haskellSuper.gtk-sni-tray.overrideAttrs (oldAttrs: {
            src = pkgs.fetchFromGitHub {
              owner = "taffybar";
              repo = "gtk-sni-tray";
              rev = "ceb15d9c0980d4359ad1b0374ba221229a14acb7";
              sha256 = "sha256-AgJGmLGNSraNr/zL+IIYF/qFUY0fEfivxfIoqIsiRWk=";
            };
            patches = [
              (pkgs.fetchpatch {
                name = "scale.patch";
                url =
                  "https://github.com/taffybar/gtk-sni-tray/commit/626d5a3ffaac1eebef033b3b52952fd95a949a8d.patch";
                sha256 = "sha256-Ml5gTWjemv3WgiTIra2zU4i+afsr3V4G55QJKV/11pM=";
              })
            ];
            prePatch = "${pkgs.hpack}/bin/hpack";
          });
          taffybar = haskellSuper.taffybar.overrideAttrs (oldAttrs: {
            src = pkgs.fetchFromGitHub {
              owner = "taffybar";
              repo = "taffybar";
              rev = "bba89541729c4da920320f93dbcb1038a8bbfe9a";
              sha256 = "sha256-tScpOIX1H3Nyp01gzJheRjK0zFFjWnEYrg9oHKrgrck=";
            };
            patches = [
              (pkgs.fetchpatch {
                name = "1.patch";
                url =
                  "https://github.com/taffybar/taffybar/commit/0efdb9f0ba4f5dc1bb05b5a5899c061b1530091c.patch";
                sha256 = "sha256-0mbWTuGF+YTlbWbGO2YADdEQbBLFu3B67MTBEJHrI8k=";
              })
              (pkgs.fetchpatch {
                name = "2.patch";
                url =
                  "https://github.com/taffybar/taffybar/commit/0a5605b657c78dfc78595b60ceeed70e4ffd75d6.patch";
                sha256 = "sha256-yKgqN5yyjIztLwm2JpT41dOXevxAMkuLCaLnAwSoSnI=";
              })
              (pkgs.fetchpatch {
                name = "tooltip.patch";
                url =
                  "https://github.com/taffybar/taffybar/commit/1b5de6bd50d5198e53de4aff7815e2943926221a.patch";
                sha256 = "sha256-lTmToEOm0VumIe88trWGJTE4szkK7ZOXnyVuhhUbso0=";
              })
            ];
          });
        };
      };
    })
  ];

  home-manager = let
    home = {
      home.file = {
        ".xinitrc".text = ''
          export _JAVA_AWT_WM_NONREPARENTING=1
          ${config.services.xserver.displayManager.sessionData.wrapper} ~/.xsession
        '';
      };
      services = {
        status-notifier-watcher.enable = true;
        taffybar = {
          enable = true;
          package = pkgs.writers.writeHaskellBin "taffybar" {
            libraries = [ pkgs.haskellPackages.taffybar ];
            ghcArgs = [ "-O2" "-threaded" "-rtsopts" "-with-rtsopts=-N" ];
          } (builtins.readFile ./taffybar/taffybar.hs);
        };
        pasystray.enable = true;
        parcellite = {
          enable = true;
          package = pkgs.clipit;
        };
        network-manager-applet.enable = true;
        dunst = {
          enable = true;
          settings = {
            global = {
              font = "DejaVu Sans 12";
              alignment = "center";
              geometry = "0x0-10+25";
              frame_width = 1;
              frame_color = "#888888";
              transparency = 30;
              idle_threshold = 60;
              monitor = 0;
            };
            urgency_low = {
              background = "#000000";
              foreground = "#888888";
              timeout = 10;
            };
            urgency_normal = {
              background = "#000000";
              foreground = "#ffffff";
              timeout = 20;
            };
            urgency_critical = {
              background = "#ff0000";
              foreground = "#ffffff";
              timeout = 0;
            };
            shortcuts = {
              close = "ctrl+space";
              close_all = "ctrl+shift+space";
              history = "ctrl+shift+grave";
            };
          };
        };
        xscreensaver = {
          enable = true;
          settings = {
            timeout = "0:10:00";
            lock = true;
            lockTimeout = "0:02:30";
            passwdTimeout = "0:00:30";
            visualID = "default";
            splash = false;
            nice = 10;
            fade = false;
            unfade = false;
            dpmsEnabled = true;
            dpmsQuickOff = true;
            dpmsStandby = "0:15:00";
            dpmsSuspend = "0:20:00";
            dpmsOff = "0:30:00";
            mode = "blank";
            selected = 206;
            pointerHysteresis = 10;
            procInterrupts = true;
            authWarningSlack = 20;
          };
        };
        picom = {
          enable = true;
          shadow = true;
          shadowOffsets = [ (-5) (-5) ];
          shadowOpacity = "0.5";
          inactiveDim = "0.2";
          fade = true;
          fadeDelta = 4;
          vSync = true;
          extraOptions = ''
            glx-no-stencil = true;
            glx-no-rebind-pixmap = true;
            shadow-radius = 5;
            mark-wmwin-focused = true;
            use-ewmh-active-win = true;
            detect-rounded-corners = true;
            detect-client-opacity = true;
            unredir-if-possible = true;
            detect-transient = true;
            detect-client-leader = true;
          '';
        };
      };
      xsession = {
        enable = true;
        importedVariables = [ "PATH" "GDK_PIXBUF_MODULE_FILE" ];
        # TODO: upstream
        profileExtra =
          "dbus-update-activation-environment DBUS_SESSION_BUS_ADDRESS DISPLAY SSH_AUTH_SOCK XAUTHORITY XDG_DATA_DIRS XDG_RUNTIME_DIR XDG_SESSION_ID PATH GDK_PIXBUF_MODULE_FILE";
        windowManager.command = "xmonad 2>> ~/.xsession-errors";
        preferStatusNotifierItems = true;
        pointerCursor = {
          package = pkgs.gnome3.adwaita-icon-theme;
          name = "Adwaita";
          size = 16;
        };
        numlock.enable = true;
      };
      # TODO: upstream this wrapper somehow
      systemd.user.services.picom.Service.ExecStart = let
        conf = pkgs.writeText "picom.conf" (builtins.readFile ./picom.conf);
        invert =
          pkgs.writeText "invert.glsl" (builtins.readFile ./glsl/negative.glsl);
      in lib.mkForce ''
        ${pkgs.bash}/bin/bash -c 'exec ${pkgs.picom}/bin/picom --config ${conf} --dbus --glx-fshader-win "$(cat ${invert})"'
      '';
    };
  in {
    users = {
      kurnevsky = home;
      ww = home;
    };
  };
}
