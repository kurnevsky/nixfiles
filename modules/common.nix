{ config, lib, pkgs, ... }:

{
  nix = {
    package = pkgs.nixFlakes;
    autoOptimiseStore = true;
    extraOptions = "experimental-features = nix-command flakes";
    binaryCaches =
      [ "https://cachix.cachix.org" "https://nix-community.cachix.org" ];
    binaryCachePublicKeys = [
      "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [
      "symbola"
      "font-bh-lucidatypewriter"
      "unrar"
      "p7zip"
    ];

  boot = {
    # TODO: use pkgs.linuxPackages_xanmod after next nixos release
    kernelPackages = pkgs.linuxPackages_zen;
    kernel.sysctl = {
      "kernel.sysrq" = 1;
      "net.ipv4.ip_forward" = 1;
    };
    tmpOnTmpfs = true;
    supportedFilesystems = [ "ntfs" ];
  };

  networking = {
    useDHCP = false;
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [
        # SSH
        22
        # VNC
        5901
        # MPD
        8000
      ];
      allowedUDPPorts = [
        # Tox
        33445
        # WireGuard
        51871
      ];
    };
    wireguard.interfaces.wg0 = {
      listenPort = 51871;
      privateKeyFile = "/secrets/wg/private.key";
      peers = [{
        endpoint = "kurnevsky.net:51871";
        publicKey = "5JHCxIYeZ50k7YJM+kLAbqGW4LAXpI5lycYEWSVxkBE=";
        presharedKeyFile = "/secrets/wg/preshared.psk";
        allowedIPs = [ "192.168.14.0/24" ];
        persistentKeepalive = 25;
      }];
    };
    extraHosts = ''
      192.168.14.2 home
      192.168.14.3 work
      192.168.14.4 parents
    '';
  };

  console = {
    font = "cyr-sun16";
    keyMap = "ru";
  };

  time.timeZone = "Europe/Minsk";

  environment = {
    systemPackages = with pkgs; [
      (agda.withPackages (pkgs: with pkgs; [ standard-library ]))
      (lutris.override { steamSupport = false; })
      (pass.withExtensions (ext: with ext; [ pass-otp ]))
      R
      aircrack-ng
      alacritty
      anki
      ansible
      ansible-lint
      aspell
      aspellDicts.en
      aspellDicts.ru
      astyle
      barcode
      bat
      bind
      bindfs
      binutils
      brightnessctl
      btrfs-progs
      bubblewrap
      cabal-install
      calibre
      cargo
      cataclysm-dda
      chromium-sandboxed
      clinfo
      coursier
      davfs2
      deadbeef-sandboxed
      docker-compose
      dosbox
      dosfstools
      e2fsprogs
      editorconfig-core-c
      eiskaltdcpp
      element-desktop-sandboxed
      exa
      exfat-utils
      extundelete
      fbreader
      fd
      fdupes
      feh-sandboxed
      ffmpeg-full-sandboxed
      firefox-sandboxed
      fuseiso
      gcc
      gdb
      ghc
      gimp-with-plugins
      git
      gitAndTools.delta
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      gparted
      graphicsmagick
      graphicsmagick-imagemagick-compat
      hans
      # haskell-language-server
      hdparm
      hedgewars
      hicolor-icon-theme # contains deadbeef icon
      hlint
      hunspell
      hunspellDicts.en_US
      hunspellDicts.ru_RU
      imv-sandboxed
      inkscape
      innoextract
      iodine
      iotop
      isync
      jq
      kafkacat
      kubectl
      languagetool
      libreoffice-fresh-sandboxed
      libva-utils
      lsd
      lshw
      maim
      maxima
      mc
      mercurial
      mesa-demos
      metals
      metasploit
      mono
      mpc_cli
      mpv-sandboxed
      mu
      nettools
      networkmanagerapplet
      newsboat
      nix-diff
      nixfmt
      ncmpc
      nmap
      nodePackages.bash-language-server
      nodePackages.prettier
      numlockx
      octave
      openconnect
      openjdk
      openmw
      openssl
      p7zip-sandboxed
      pandoc # TODO: it should depend on texlive
      parallel
      pavucontrol
      perl
      pidgin-sandboxed
      pijul
      playerctl
      psmisc # for killall
      qbittorrent-sandboxed
      qemu
      qrencode
      qtox-sandboxed
      radare2
      rclone
      ripgrep
      ripgrep-all
      rsync
      rust-analyzer
      rustc
      sbt
      scala
      shellcheck
      skim
      smartmontools
      sourceHighlight
      subversion
      tealdeer
      tdesktop-sandboxed
      telegram-purple
      tesseract
      texlive.combined.scheme-basic
      tigervnc
      tinc
      tmux
      tor-browser-bundle-bin-wrapped
      torsocks
      toxic-sandboxed
      unrar-sandboxed
      unzip-natspec-sandboxed
      vdpauinfo
      viu
      vlc-sandboxed
      vulkan-tools
      websocat
      wesnoth
      wget
      wine-staging-sandboxed
      winetricks
      wireguard-tools
      wmctrl
      wxmaxima
      xcalib
      xcb-client-id
      xdotool
      xmlstarlet
      xmobar
      xorg.xbacklight
      xsel
      xterm
      you-get
      youtube-dl
      zathura-sandboxed
      zbar
      # findimagedupes
      # tuntox
      # vagrant
      # veloren
      # virtualbox
      # tensorflow
      # sane xsane
    ];

    shellAliases = {
      ls = "ls --color=auto";
      grep = "grep --color=auto";
      su = "sudo su";
    };

    variables = {
      ALTERNATE_EDITOR = "nano";
      VIEWER = "less";
      # It causes segfaults
      MAGICK_OCL_DEVICE = "OFF";
    };

    etc."taffybar.css".source = ./taffybar/taffybar.css;
  };

  security.wrappers.xscreensaver-auth.source =
    "${pkgs.xscreensaver}/libexec/xscreensaver/xscreensaver-auth";

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "Hack" ]; })
    noto-fonts
    noto-fonts-extra
    noto-fonts-emoji
    symbola
  ];

  programs = {
    gnupg.agent.enable = true;
    zsh = {
      enable = true;
      enableCompletion = true;
      # will be enabled in interactiveShellInit differently
      enableGlobalCompInit = false;
      autosuggestions = {
        enable = true;
        extraConfig = { ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE = "10"; };
      };
      syntaxHighlighting.enable = true;
      # Override default prompt as it doesn't work good in Midnigt Commander
      promptInit = "PROMPT='[%n@%m %~]$ '";
      histSize = 25000;
      histFile = "~/.histfile";
      setOptions = [
        # Remove all duplicates of current command from history, add current to end
        "hist_ignore_all_dups"
        # Don't save any commands beginning with space
        "hist_ignore_space"
        # Enable extended globs to interpret things like rm ^(file|file2)
        "extended_glob"
        # Don't beep even if zsh don't like something
        "no_beep"
        # Change directory even if user forgot to put 'cd' command in front, but entered path is valid
        "auto_cd"
        # If possible, correct commands
        "correct"
        # Append their history list to the history file, rather than replace it
        "inc_append_history"
        # If a pattern for filename generation has no matches, print an error, instead of leaving it unchanged in the argument list
        "nomatch"
        # Report the status of background jobs immediately, rather than waiting until just before printing a prompt
        "notify"
        # Allow parameter expansion, command substitution and arithmetic expansion for prompt string
        "prompt_subst"
        # Remove any right prompt from display when accepting a command line
        "transient_rprompt"
        # File completion after =
        "magic_equal_subst"
        # Apply globbing to hidden files
        "glob_dots"
        # Use OS file locking
        "hist_fcntl_lock"
      ];
      shellAliases = {
        zmv = "noglob zmv -W";
        calc = "noglob zcalc -e";
      };
      interactiveShellInit = builtins.readFile ./interactive-init.zsh;
    };
    adb.enable = true;
    less = {
      enable = true;
      lessopen = "| ${pkgs.sourceHighlight}/bin/src-hilite-lesspipe.sh %s";
      envVariables.LESS = " -R ";
    };
    qt5ct.enable = true;
  };

  gtk.iconCache.enable = true;

  services = {
    i2pd = {
      enable = true;
      proto = {
        http.enable = true;
        socksProxy.enable = true;
      };
    };
    monero.enable = true;
    openssh = {
      enable = true;
      forwardX11 = true;
    };
    printing.enable = true;
    resolved.enable = true;
    tlp.enable = true;
    tor = {
      enable = true;
      client = {
        enable = true;
        transparentProxy.enable = true;
        dns.enable = true;
      };
      settings.Socks5Proxy = "127.0.0.1:1080";
    };
    timesyncd.enable = true;
    upower.enable = true;
    xserver = {
      enable = true;
      # causes GDK_PIXBUF_MODULE_FILE to be set in xsession
      gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
      layout = "us,ru";
      xkbOptions = "grp:caps_toggle,grp_led:caps,terminate:ctrl_alt_bksp";
      libinput = {
        enable = true;
        touchpad.disableWhileTyping = true;
      };
      displayManager = {
        xserverArgs = [ "-nolisten local" ];
        startx.enable = true;
      };
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = pkgs: with pkgs; [ dbus regex-compat taffybar ];
        config = builtins.readFile ./xmonad/xmonad.hs;
        ghcArgs = [ "-O2" "${./xmonad/lib/XMonad/Util/Compton.hs}" ];
      };
    };
    emacs = {
      enable = true;
      defaultEditor = true;
      package = pkgs.emacsPatched;
    };
    mpd = {
      enable = true;
      startWhenNeeded = true;
      extraConfig = ''
        audio_output {
          type "httpd"
          name "My HTTP Stream"
          encoder "vorbis"
          port "8000"
          bind_to_address "0.0.0.0"
          bitrate "192"
          format "44100:16:1"
          max_clients "0"
        }
      '';
    };
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  hardware = {
    bluetooth = {
      enable = true;
      package = pkgs.bluezFull;
    };
    usbWwan.enable = true;
    pulseaudio = {
      enable = true;
      package = pkgs.pulseaudioFull;
      support32Bit = true;
    };
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
  };

  systemd = {
    user.services.dbus.wantedBy = [ "default.target" ];
    services = {
      i2pd.wantedBy = pkgs.lib.mkForce [ ];
      monero.wantedBy = pkgs.lib.mkForce [ ];
      tor.wantedBy = pkgs.lib.mkForce [ ];
    };
  };

  security = {
    # Enable pam_systemd module to set dbus environment variable.
    pam.services.login.startSession = true;
    unprivilegedUsernsClone = true;
    rtkit.enable = true;
    sudo.extraRules = [{
      runAs = "root";
      users = [ "ww" ];
      commands =
        [ "/run/current-system/sw/bin/ip netns exec torjail sudo -u ww [!-]*" ];
    }];
  };

  # system.replaceRuntimeDependencies can be used to make fast fixes
  nixpkgs.overlays = let
    optimizeWithFlag = pkg: flag:
      pkg.overrideAttrs (attrs: {
        NIX_CFLAGS_COMPILE = (attrs.NIX_CFLAGS_COMPILE or "") + " ${flag}";
      });
    optimizeWithFlags = pkg: flags:
      pkgs.lib.foldl' (pkg: flag: optimizeWithFlag pkg flag) pkg flags;
    optimizeForThisHost = pkg:
      optimizeWithFlags pkg [ "-O3" "-march=native" "-mtune=native" ];
  in [
    (self: super: {
      xscreensaver = super.xscreensaver.overrideAttrs (attrs: {
        patches = (super.xscreensaver.patches or [ ])
          ++ [ ./xscreensaver-hack.diff ];
      });
    })
    (self: super: {
      uutils-coreutils = super.uutils-coreutils.override { prefix = null; };
    })
    (self: super:
      let
        mc = optimizeForThisHost (super.mc.override {
          zip = super.zip-natspec-sandboxed;
          unzip = super.unzip-natspec-sandboxed;
        });
      in {
        # TODO: don't wrap after next nixos release
        mc = pkgs.writeShellScriptBin "mc" ''
          export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.xorg.libX11}/lib
          exec ${mc}/bin/mc $@
        '';
      })
    (self: super: {
      tesseract =
        super.tesseract.override { enableLanguages = [ "eng" "rus" ]; };
    })
    (self: super: { wine = super.wineWowPackages.staging; })
    (self: super: {
      emacsPatched = super.emacs.overrideAttrs (oldAttrs: {
        patches = (oldAttrs.patches or [ ]) ++ [
          (pkgs.fetchpatch {
            name = "antifreeze.patch";
            url =
              "https://github.com/emacs-mirror/emacs/commit/c36df52ff5c05826382d88ddbe3fffaa99d12597.patch";
            sha256 = "sha256-adPgvhiHB2MyRE/8WYD5misXtuMSPElDsyrX5WOqxbQ=";
          })
        ];
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

  users = {
    mutableUsers = false;
    motd = "Abandon all hope, ye who enter here.";
    users = {
      # To get hash use:
      # openssl passwd -6 password
      root = {
        shell = pkgs.zsh;
        hashedPassword = "!";
      };
      kurnevsky = {
        uid = 1000;
        isNormalUser = true;
        extraGroups = [ "wheel" "adbusers" "audio" "video" ];
        shell = pkgs.zsh;
        passwordFile = "/secrets/kurnevsky";
      };
      ww = {
        uid = 1001;
        isNormalUser = true;
        shell = pkgs.zsh;
        passwordFile = "/secrets/ww";
      };
    };
  };

  home-manager = let
    bash = {
      initExtra = ''
        if [[ $TERM != "dumb" && -z "$MC_SID" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "vterm") ]]; then
          eval "$(${pkgs.starship}/bin/starship init bash)"
        fi
      '';
    };
    zsh = {
      enable = true;
      enableCompletion = false;
      history = {
        size = 20000;
        save = 25000;
        path = "$HOME/.histfile";
        expireDuplicatesFirst = true;
        share = false;
        ignoreDups = false;
      };
      initExtra = ''
        if [[ $TERM != "dumb" && -z "$MC_SID" && (-z $INSIDE_EMACS || $INSIDE_EMACS == "vterm") ]]; then
          eval "$(${pkgs.starship}/bin/starship init zsh)"
        fi
      '';
    };
    starship = {
      enable = true;
      # Enabled manually since there is no way to disable it for mc
      enableBashIntegration = false;
      enableZshIntegration = false;
      enableFishIntegration = false;
      settings = {
        add_newline = false;
        character = {
          success_symbol = "[➜](bold green)";
          error_symbol = "[➜](bold red)";
        };
        directory = {
          truncation_length = 0;
          truncate_to_repo = false;
        };
      };
    };
    feh = {
      enable = true;
      buttons = {
        zoom_in = "C-4";
        zoom_out = "C-5";
      };
    };
    tmux = {
      enable = true;
      terminal = "screen-256color";
      historyLimit = 10000;
      extraConfig = builtins.readFile ./tmux.conf;
    };
    alacritty = {
      enable = true;
      settings = {
        window.decorations = "none";
        scrolling.history = 100000;
        font = {
          normal.family = "Hack Nerd Font";
          size = 12;
        };
        # Base16 OneDark
        colors = {
          primary = {
            background = "0x282c34";
            foreground = "0xabb2bf";
          };
          cursor = {
            text = "0x282c34";
            cursor = "0xabb2bf";
          };
          normal = {
            black = "0x282c34";
            red = "0xe06c75";
            green = "0x98c379";
            yellow = "0xe5c07b";
            blue = "0x61afef";
            magenta = "0xc678dd";
            cyan = "0x56b6c2";
            white = "0xabb2bf";
          };
          bright = {
            black = "0x545862";
            red = "0xd19a66";
            green = "0x353b45";
            yellow = "0x3e4451";
            blue = "0x565c64";
            magenta = "0xb6bdca";
            cyan = "0xbe5046";
            white = "0xc8ccd4";
          };
        };
        draw_bold_text_with_bright_colors = false;
        cursor.style = "Beam";
        live_config_reload = false;
        hints.enabled = [{
          regex = ''
            (ipfs:|ipns:|magnet:|mailto:|gemini:|gopher:|https:|http:|news:|file:|git:|ssh:|ftp:)[^\u0000-\u001F\u007F-\u009F<>"\\s{-}\\^⟨⟩`]+'';
          command = "xdg-open";
          post_processing = true;
          mouse = {
            enabled = true;
            mods = "Control";
          };
          binding = {
            key = "U";
            mods = "Control|Shift";
          };
        }];
        key_bindings = [
          {
            key = "Insert";
            mods = "Control";
            action = "Copy";
          }
          {
            key = "Home";
            chars = "\\x1bOH";
          }
          {
            key = "End";
            chars = "\\x1bOF";
          }
          {
            key = "B";
            mods = "Control|Shift";
            action = "SearchForward";
          }
          {
            key = "F";
            mods = "Control|Shift";
            action = "SearchBackward";
          }
          {
            key = "Home";
            mods = "Control";
            mode = "Vi";
            action = "ScrollToTop";
          }
          {
            key = "End";
            mods = "Control";
            mode = "Vi";
            action = "ScrollToBottom";
          }
          {
            key = "PageUp";
            mode = "Vi";
            action = "ScrollPageUp";
          }
          {
            key = "PageDown";
            mode = "Vi";
            action = "ScrollPageDown";
          }
          {
            key = "Space";
            mode = "Vi";
            action = "ToggleNormalSelection";
          }
          {
            key = "Q";
            mode = "Vi";
            action = "ToggleViMode";
          }
        ];
      };
    };
    root = {
      home.file.".config/mc/ini".source = ./mc.ini;
      programs = {
        inherit bash;
        inherit zsh;
        inherit starship;
        inherit tmux;
      };
    };
    home = {
      home.file = {
        ".config/mc/ini".source = ./mc.ini;
        ".xinitrc".text = ''
          export _JAVA_AWT_WM_NONREPARENTING=1
          ${config.services.xserver.displayManager.sessionData.wrapper} ~/.xsession
        '';
      };
      programs = {
        inherit bash;
        inherit zsh;
        inherit starship;
        inherit tmux;
        inherit feh;
        inherit alacritty;
      };
      services = {
        status-notifier-watcher.enable = true;
        gnome-keyring.enable = true;
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
      xresources.properties = import ./xresources.nix;
      xdg.mimeApps = {
        enable = true;
        defaultApplications = import ./default-applications.nix;
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
    useGlobalPkgs = true;
    useUserPackages = true;
    users = {
      root = root;
      kurnevsky = home;
      ww = home;
    };
  };
}
