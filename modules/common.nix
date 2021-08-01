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
    kernel.sysctl = { "kernel.sysrq" = 1; };
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
      haskell-language-server
      hdparm
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
      motion
      mpd
      mpv-sandboxed
      mu
      nettools
      networkmanagerapplet
      newsboat
      nix-diff
      nixfmt
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
      picom
      pidgin-sandboxed
      pijul
      playerctl
      psmisc # for killall
      qbittorrent-sandboxed
      qemu
      qrencode
      qt5ct
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
      trayer
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
  };

  gtk.iconCache.enable = true;

  services = {
    i2pd.enable = true;
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
        # config = pkgs.builtins.readFile "/etc/xmonad.hs";
      };
    };
    emacs = {
      enable = true;
      defaultEditor = true;
      package = pkgs.emacsPatched;
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
                rev = "v0.3.1.0";
                sha256 = "sha256-teycP5kmokSCxtJjRaYEGx8DWnGKKO6qQG37iqdIdEM=";
              };
              patches = [
                (pkgs.fetchpatch {
                  name = "item-is-menu.patch";
                  url =
                    "https://github.com/taffybar/status-notifier-item/commit/4a22a449abd30695bb37ebc0fc357f4d547f5cef.patch";
                  sha256 =
                    "sha256-Q8vE5/eVtuX+v82qTu6PqTG/fyIw5zziFP8UghVEYUs=";
                })
              ];
              prePatch = "${pkgs.hpack}/bin/hpack";
            });
          gtk-sni-tray = haskellSuper.gtk-sni-tray.overrideAttrs (oldAttrs: {
            src = pkgs.fetchFromGitHub {
              owner = "taffybar";
              repo = "gtk-sni-tray";
              rev = "1734aa999ebace650232a0176643cea719ea6d5f";
              sha256 = "sha256-08nT4sergSy5hj/5ytwaoKaZs0gg8Qyl5OL/hnKkMnQ=";
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
              rev = "5a59586d7b715cb088f67634c5be096bb4f3df56";
              sha256 = "sha256-MRds7ll6VTLTy6k62mWtzkzEewk73EXFGlW7mecAfjk=";
            };
            patches = [ ];
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
    common = {
      programs = {
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
        };
        starship.enable = true;
      };
    };
    root = common // { home.stateVersion = "21.05"; };
    home = common // {
      home.stateVersion = "21.05";
      services = {
        status-notifier-watcher.enable = true;
        gnome-keyring.enable = true;
        taffybar.enable = true;
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
      };
      xsession = {
        enable = true;
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
