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
    ];

  # networking.hostName = "nixos";
  # networking.wireless.enable = true;

  networking.useDHCP = false;
  networking.networkmanager.enable = true;

  console = {
    font = "cyr-sun16";
    keyMap = "ru";
  };

  time.timeZone = "Europe/Minsk";

  environment.systemPackages = with pkgs; [
    (agda.withPackages (pkgs: with pkgs; [ standard-library ]))
    (pass.withExtensions (ext: with ext; [ pass-otp ]))
    (pkgs.callPackage ./xcb-client-id.nix { })
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
    jetbrains.idea-community
    jq
    kafkacat
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
    ntfs3g
    numlockx
    octave
    openconnect
    openjdk
    openmw
    openssl
    p7zip-sandboxed
    pandoc # TODO: it should depend on texlive
    parallel
    passff-host
    pavucontrol
    perl
    picom
    pidgin-sandboxed
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
    tdesktop-sandboxed
    telegram-purple
    tesseract
    texlive.combined.scheme-basic
    tigervnc
    tinc
    tldr
    tmux
    tor-browser-bundle-bin-wrapped
    torsocks
    toxic-sandboxed
    trayer
    unrar-sandboxed
    unzip-natspec-sandboxed
    uutils-coreutils
    vdpauinfo
    viu
    vlc-sandboxed
    vscodium
    websocat
    wesnoth
    wget
    wine-staging-sandboxed
    winetricks
    wireguard-tools
    wmctrl
    wxmaxima
    xcalib
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
    # uutils-coreutils
    # vagrant
    # veloren
    # virtualbox
    # tensorflow
    # sane xsane
  ];

  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "Hack" ]; })
    noto-fonts
    noto-fonts-extra
    noto-fonts-emoji
    symbola
  ];

  programs.gnupg.agent.enable = true;

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
  };

  programs.adb.enable = true;

  gtk.iconCache.enable = true;

  services = {
    haveged.enable = true;
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
    xserver = {
      enable = true;
      # causes GDK_PIXBUF_MODULE_FILE to be set in xsession
      gdk-pixbuf.modulePackages = [ pkgs.librsvg ];
      # Enable touchpad support.
      # libinput.enable = true;
      layout = "us,ru";
      xkbOptions = "grp:caps_toggle,grp_led:caps,terminate:ctrl_alt_bksp";
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
    };
  };

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 ];
    allowedUDPPorts = [ 33445 ];
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluezFull;
  };

  hardware.usbWwan.enable = true;

  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [ intel-media-driver vaapiVdpau libvdpau-va-gl ];
  };

  systemd.user.services.dbus.wantedBy = [ "default.target" ];
  systemd.services.i2pd.wantedBy = pkgs.lib.mkForce [ ];
  systemd.services.monero.wantedBy = pkgs.lib.mkForce [ ];
  systemd.services.tor.wantedBy = pkgs.lib.mkForce [ ];

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
    (self: super: rec {
      mc = optimizeForThisHost (super.mc.override {
        zip = super.zip-natspec-sandboxed;
        unzip = super.unzip-natspec-sandboxed;
      });
    })
    (self: super: {
      tesseract =
        super.tesseract.override { enableLanguages = [ "eng" "rus" ]; };
    })
    (self: super: { wine = super.wineWowPackages.staging; })
    (self: super: {
      emacs = super.emacs.overrideAttrs (oldAttrs: {
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
        };
      };
    })
  ];

  users = {
    mutableUsers = false;
    users = {
      # To get hash use:
      # openssl passwd -6 password
      root.hashedPassword = "!";
      kurnevsky = {
        uid = 1000;
        isNormalUser = true;
        extraGroups = [ "wheel" "adbusers" ];
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
        xembed-sni-proxy.enable = true;
        taffybar.enable = true;
        pasystray.enable = true;
        parcellite = {
          enable = true;
          package = pkgs.clipit;
        };
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
    users.kurnevsky = home;
    users.ww = home;
  };
}
