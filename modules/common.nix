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
      starship
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
  };

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
      # will be enabled in interactiveShellInit differently
      enableCompletion = false;
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
      interactiveShellInit = builtins.readFile ./interactive-init.zsh;
    };
    adb.enable = true;
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
    tor.enable = true;
    timesyncd.enable = true;
    upower.enable = true;
    xserver = {
      enable = true;
      # causes GDK_PIXBUF_MODULE_FILE to be set in xsession
      gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
      # Enable touchpad support.
      # libinput.enable = true;
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
    };
    opengl = {
      enable = true;
      extraPackages = with pkgs; [
        intel-media-driver
        vaapiVdpau
        libvdpau-va-gl
      ];
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
      uutils-coreutils = super.uutils-coreutils.override { prefix = null; };
    })
    (self: super:
      let
        mc = optimizeForThisHost (super.mc.override {
          zip = super.zip-natspec-sandboxed;
          unzip = super.unzip-natspec-sandboxed;
        });
      in {
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
          gtk-sni-tray = haskellSuper.gtk-sni-tray.overrideAttrs (oldAttrs: {
            patches = (oldAttrs.patches or [ ]) ++ [
              (pkgs.fetchpatch {
                name = "scroll.patch";
                url =
                  "https://github.com/taffybar/gtk-sni-tray/commit/f7af7d00660790fb7143fea5b48e4f83765b3730.patch";
                sha256 = "sha256-Ap83YnmaPvziST76MWlBKM28+QrG6Vza9btMxLpeOCQ=";
              })
              (pkgs.fetchpatch {
                name = "mouse.patch";
                url =
                  "https://github.com/taffybar/gtk-sni-tray/commit/af631502f89d9686a84e7ab49e8b01b95a817eed.patch";
                sha256 = "sha256-xJgpZhH7NZzw0VwKYbkXEPrwr+CURL11fpQJRVTMH9g=";
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
    home = {
      home.stateVersion = "21.05";
      services = {
        status-notifier-watcher.enable = true;
        gnome-keyring.enable = true;
        xembed-sni-proxy.enable = true;
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
      xresources.properties = let
        base00 = "#282c34";
        base01 = "#353b45";
        base02 = "#3e4451";
        base03 = "#545862";
        base04 = "#565c64";
        base05 = "#abb2bf";
        base06 = "#b6bdca";
        base07 = "#c8ccd4";
        base08 = "#e06c75";
        base09 = "#d19a66";
        base0A = "#e5c07b";
        base0B = "#98c379";
        base0C = "#56b6c2";
        base0D = "#61afef";
        base0E = "#c678dd";
        base0F = "#be5046";
      in {
        "*foreground" = base05;
        "*background" = base00;
        "*cursorColor" = base05;
        "*color0" = base00;
        "*color1" = base08;
        "*color2" = base0B;
        "*color3" = base0A;
        "*color4" = base0D;
        "*color5" = base0E;
        "*color6" = base0C;
        "*color7" = base05;
        "*color8" = base03;
        "*color9" = base09;
        "*color10" = base01;
        "*color11" = base02;
        "*color12" = base04;
        "*color13" = base06;
        "*color14" = base0F;
        "*color15" = base07;
        "Xft.dpi" = 96;
        "Xft.antialias" = true;
        "Xft.rgba" = "rgb";
        "Xft.hinting" = true;
        "Xft.hintstyle" = "hintslight";
        "XTerm.termName" = "xterm-256color";
        "XTerm.vt100.saveLines" = 0;
        "XTerm.vt100.reverseVideo" = false;
        "XTerm.vt100.faceName" =
          "xft:DejaVu Sans Mono:size=12:antialias=true:autohint=false";
        "XTerm.vt100.bellIsUrgent" = true;
        "XTerm.vt100.metaSendsEscape" = true;
        # Send C-? instead of C-h for the backspace key
        "XTerm.ttyModes" = "erase ^?";
        "XTerm.vt100.translations" = "#override <Key>BackSpace: string(0x7f)";
        "XTerm.backarrowKeyIsErase" = true;
      };
    };
  in {
    useGlobalPkgs = true;
    useUserPackages = true;
    users = {
      kurnevsky = home;
      ww = home;
    };
  };
}
