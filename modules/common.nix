{ config, lib, pkgs, nixpkgs-unstable, ... }:

{
  nix = {
    package = pkgs.nixFlakes;
    autoOptimiseStore = true;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
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
    kernelPackages = pkgs.linuxPackages_xanmod;
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
      (lutris.override { steamSupport = false; })
      (pass-wayland.withExtensions (ext: with ext; [ pass-otp ]))
      (pkgs.callPackage ./pan-globalprotect-okta.nix { })
      (pkgs.callPackage ./webcam-filters.nix { })
      aircrack-ng
      alacritty
      anki
      ansible
      ansible-lint
      aspell
      aspellDicts.en
      aspellDicts.ru
      barcode
      bat
      bind
      bindfs
      binutils
      blender
      brightnessctl
      btrfs-progs
      bubblewrap
      calibre
      clinfo
      cuetools
      davfs2
      deadbeef-with-plugins
      docker-compose
      dosbox
      dosfstools
      e2fsprogs
      eiskaltdcpp
      exa
      exfat-utils
      extundelete
      fd
      fclones
      ffmpeg-full
      flac
      fuseiso
      gdb
      gimp-with-plugins
      gnome-themes-extra
      gnome3.adwaita-icon-theme
      gparted
      graphicsmagick
      graphicsmagick-imagemagick-compat
      hans
      hdparm
      hicolor-icon-theme # contains deadbeef icon
      hunspell
      hunspellDicts.en_US
      hunspellDicts.ru_RU
      imv
      inkscape
      innoextract
      iodine
      iotop
      isync
      jq
      kafkacat
      kubectl
      languagetool
      libreoffice-fresh
      libva-utils
      libxml2 # for xmllint
      lsd
      lshw
      maim
      mc
      mesa-demos
      metasploit
      mpc_cli
      mu
      nettools
      newsboat
      nix-diff
      nixfmt
      ncmpc
      nmap
      nodePackages.prettier
      openconnect
      openssl
      pandoc # TODO: it should depend on texlive
      parallel
      pavucontrol
      pulseaudio # for pactl
      perl
      playerctl
      psmisc # for killall
      qbittorrent
      qemu
      qrencode
      radare2
      rclone
      ripgrep
      ripgrep-all
      rsync
      shntool
      skim
      smartmontools
      sourceHighlight
      tealdeer
      tesseract
      texlive.combined.scheme-basic
      tigervnc
      tinc
      tmux
      torsocks
      usbutils
      vdpauinfo
      viu
      vlc
      vorbis-tools
      vulkan-tools
      wavpack
      websocat
      wget
      wineWowPackages.stagingFull
      winetricks
      wireguard-tools
      wirelesstools
      xmlstarlet
      you-get
      youtube-dl
      zathura
      zbar
      # Archivers
      _7zz
      p7zip
      unrar
      unzip-natspec
      # Browsers
      tor-browser-bundle-bin
      # Messengers
      pidgin-with-plugins
      tdesktop
      ## Tox
      qtox
      toxic
      # Games
      cataclysm-dda
      hedgewars
      openmw
      wesnoth
      # Languages
      (agda.withPackages (pkgs: with pkgs; [ standard-library ]))
      astyle
      gcc
      mono
      nodePackages.bash-language-server
      openjdk
      python3
      shellcheck
      ## Haskell
      cabal-install
      ghc
      haskell-language-server
      hlint
      ## Rust
      (fenix.stable.withComponents [
        "cargo"
        "clippy"
        "rust-src"
        "rustc"
        "rustfmt"
      ])
      rust-analyzer
      ## Scala
      coursier
      metals
      sbt
      scala
      ## Math
      maxima
      wxmaxima
      octave
      R
      # VCS
      git
      gitAndTools.delta
      mercurial
      pijul
      subversion
      # TODO
      # findimagedupes
      # tuntox
      # vagrant
      # veloren
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
    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };
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
    emacs = {
      enable = true;
      defaultEditor = true;
      package = (pkgs.emacsWithPackagesFromUsePackage {
        config = ./init.el;
        package = pkgs.emacsGcc;
        alwaysEnsure = true;
        extraEmacsPackages = epkgs: [
          (pkgs.stdenv.mkDerivation {
            name = "hexrgb.el";
            src = pkgs.fetchurl {
              url = "https://www.emacswiki.org/emacs/download/hexrgb.el";
              sha256 = "sha256-qFKdT3IjhUAgNaCVTjCe2cT8rOqfPSdbVCL14/JCC6I=";
            };
            unpackCmd = "mkdir el && cp $curSrc el/hexrgb.el";
            buildPhase =
              "${pkgs.emacs}/bin/emacs -Q -nw -batch -f batch-byte-compile hexrgb.el";
            installPhase =
              "mkdir -p $out/share/emacs/site-lisp && install *.el* $out/share/emacs/site-lisp";
          })
          epkgs.fringe-helper # for revive origami
        ];
        override = epkgs:
          epkgs // {
            origami = epkgs.melpaPackages.origami.overrideAttrs (old: {
              src = pkgs.fetchFromGitHub {
                owner = "elp-revive";
                repo = "origami.el";
                rev = "adf3cf1b813d93cfd7153a3e3799232e8545366b";
                sha256 = "sha256-lnK3+o9m7qBs82UTp6qQ8kRwP7js2WI2wlxi9jOF4Jw=";
              };
            });
          };
      });
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

  virtualisation = {
    docker = {
      enable = true;
      enableOnBoot = false;
    };
    virtualbox.host.enable = true;
  };

  hardware = {
    bluetooth = {
      enable = true;
      package = pkgs.bluezFull;
    };
    usbWwan.enable = true;
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
  nixpkgs.overlays = [
    (self: super: {
      uutils-coreutils = super.uutils-coreutils.override { prefix = null; };
    })
    (self: super: { wine = super.wineWowPackages.stagingFull; })
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
        extraGroups = [ "wheel" "adbusers" "audio" "video" "vboxusers" ];
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
    mpv = {
      enable = true;
      config = {
        hwdec = "auto";
        cache = true;
        demuxer-max-bytes = 41943040;
        demuxer-max-back-bytes = 41943040;
        volume-max = 500;
      };
      bindings = {
        "Ctrl+n" = ''af toggle "lavfi=[dynaudnorm=f=175:g=25:p=0.75]"'';
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
    git = {
      enable = true;
      userName = "Evgeny Kurnevsky";
      userEmail = "kurnevsky@gmail.com";
      signing = {
        key = null;
        signByDefault = true;
      };
      aliases = {
        lol = "log --graph --decorate --pretty=oneline --abbrev-commit --all";
      };
      delta.enable = true;
      lfs.enable = true;
      extraConfig = {
        push.default = "simple";
        merge.conflictstyle = "diff3";
        pull.ff = "only";
        github.user = "kurnevsky";
        gitlab.user = "kurnevsky";
        gitlab."gitlab.evolutiongaming.com/api/v4".user = "ykurneuski";
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
        ".config/emacs/init.el" = {
          source = ./init.el;
          onChange = ''
            rm -fv ~/.config/emacs/init.elc
            emacs -Q -nw -l ~/.config/emacs/init.el -batch -f batch-byte-compile ~/.config/emacs/init.el
          '';
        };
      };
      programs = {
        inherit bash;
        inherit zsh;
        inherit starship;
        inherit tmux;
        inherit feh;
        inherit mpv;
        inherit alacritty;
        inherit git;
      };
      services = {
        gnome-keyring.enable = true;
        # Enable together with programs.gnupg because it handles pinentry
        gpg-agent = {
          enable = true;
          defaultCacheTtl = 14400;
          maxCacheTtl = 14400;
          pinentryFlavor = "gnome3";
        };
      };
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
