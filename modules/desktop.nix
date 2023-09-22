{ pkgs, ... }:

{
  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [ "threadirqs" ];
    kernel.sysctl."kernel.sysrq" = 1;
    tmp.useTmpfs = true;
    supportedFilesystems = [ "ntfs" ];
    postBootCommands = ''
      echo 2048 > /sys/class/rtc/rtc0/max_user_freq
      echo 2048 > /proc/sys/dev/hpet/max-user-freq
    '';
  };

  networking = {
    useDHCP = false;
    useNetworkd = true;
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedUDPPorts = [
        # Tox
        33445
        # WireGuard
        51871
      ];
      trustedInterfaces = [ "wg0" "icmp" "dns0" ];
    };
    wireguard.enable = true;
  };

  i18n.supportedLocales =
    [ "C.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" "ru_RU.UTF-8/UTF-8" ];

  console = {
    font = "cyr-sun16";
    keyMap = "ru";
  };

  time.timeZone = "Europe/Minsk";

  environment = {
    systemPackages = with pkgs; [
      (let
        obs-wrapped = (wrapOBS {
          plugins = with obs-studio-plugins; [
            obs-gstreamer
            obs-backgroundremoval
          ];
        });
        obs-with-gstreamer = stdenv.mkDerivation {
          name = "obs-with-gstreamer";
          buildInputs = with gst_all_1; [
            makeWrapper
            gstreamer
            gst-plugins-base
            gst-plugins-good
            gst-plugins-bad
            gst-plugins-ugly
            gst-libav
          ];
          buildCommand = ''
            mkdir -p $out/bin
            makeWrapper ${obs-wrapped}/bin/obs $out/bin/obs \
              --prefix GST_PLUGIN_SYSTEM_PATH_1_0 ":" "$GST_PLUGIN_SYSTEM_PATH_1_0"
          '';
        };
      in symlinkJoin {
        name = "obs-with-gstreamer-joined";
        paths = [ obs-with-gstreamer obs-wrapped ];
      })
      (pass-wayland.withExtensions (ext: with ext; [ pass-otp pass-update ]))
      (callPackage ./ical2org.nix { })
      (python311Packages.callPackage ./openhrv.nix { })
      aircrack-ng
      anki
      ansible
      ansible-lint
      appimage-run
      aria2
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
      claws-mail
      clinfo
      cloud-mdir-sync
      cuetools
      curlHTTP3
      davfs2
      deadbeef-with-plugins
      dmidecode
      docker-compose
      dosbox
      dosfstools
      e2fsprogs
      eiskaltdcpp
      eza
      exfat
      extundelete
      fd
      fclones
      feh
      ffmpeg-full
      flac
      fuseiso
      gdb
      gimp-with-plugins
      gnupg_patched
      openconnect
      (callPackage ./pan-globalprotect-okta.nix { })
      gnome-themes-extra
      gnome.adwaita-icon-theme
      gparted
      graphicsmagick
      graphicsmagick-imagemagick-compat
      hdparm
      hicolor-icon-theme # contains deadbeef icon
      hunspell
      hunspellDicts.en_US
      hunspellDicts.ru_RU
      imv
      inetutils
      inkscape
      innoextract
      iotop
      isync
      jq
      kubectl
      languagetool
      lapce
      libnotify
      libreoffice-fresh
      libva-utils
      libxml2 # for xmllint
      lm_sensors
      lsd
      lshw
      mesa-demos
      metasploit
      mpc_cli
      mpv
      mu
      nettools
      newsboat
      ncmpc
      nmap
      nodePackages.mermaid-cli
      (pkgs.writeShellScriptBin "prettier" ''
        ${pkgs.nodePackages.prettier}/bin/prettier --plugin-search-dir "${pkgs.nodePackages.prettier-plugin-toml}/lib" "$@"
      '')
      openai-whisper-cpp
      openssl
      pandoc
      parallel
      pavucontrol
      pciutils
      pulseaudio # for pactl
      perl
      playerctl
      psmisc # for killall
      qbittorrent
      qemu
      qrencode
      radare2
      rage
      rclone
      ripgrep
      rsync
      shntool
      skim
      smartmontools
      sshfs-fuse
      tealdeer
      tesseract
      texlive.combined.scheme-basic
      tigervnc
      tmux
      torsocks
      translate-shell
      ubpm
      usbutils
      v4l-utils
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
      dxvk.out
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
      (unzip.override { enableNLS = true; })
      # Browsers
      firefox
      ungoogled-chromium
      tor-browser-bundle-bin
      # Messengers
      gajim
      konversation
      tdesktop
      ## Matrix
      element-desktop
      nheko
      ## Tox
      qtox
      toxic
      # Games
      cataclysm-dda
      hedgewars
      minetest
      openmw
      wesnoth
      # Databases
      cassandra_4
      grafana-loki
      kcat
      # Wallets
      electrum
      monero-cli
      # TODO: feather-wallet
      # Audio
      easyeffects
      helvum
      qpwgraph
      sox
      # Languages
      (agda.withPackages (pkgs: with pkgs; [ standard-library ]))
      astyle
      gcc
      clang
      clang-tools
      mono
      (python3.withPackages (pkgs:
        with pkgs; [
          transformers
          pytorch
          sentencepiece
          sacremoses
          torchvision
          (callPackage ./python-diffusers.nix { })
          (callPackage ./python-accelerate.nix { })
        ]))
      groovy
      ## Shell
      nodePackages.bash-language-server
      shellcheck
      shfmt
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
      ## Java
      jdt-language-server
      maven
      ## Scala
      coursier
      sbt
      scala
      metals
      ## Nix
      alejandra
      nixfmt
      nixpkgs-fmt
      nil
      nix-diff
      nixpkgs-review
      ## Math
      maxima
      wxmaxima
      octave
      (rWrapper.override { packages = with rPackages; [ styler ggplot2 ]; })
      ## Markdown
      nodePackages.markdownlint-cli
      marksman
      ## TypeScript
      nodePackages.typescript-language-server
      # VCS
      mercurial
      pijul
      subversion
    ];

    variables = {
      EMAIL = "kurnevsky@gmail.com";
      ALTERNATE_EDITOR = "nano";
      VIEWER = "less";
      # It causes segfaults
      MAGICK_OCL_DEVICE = "OFF";
    };

    sessionVariables = {
      NIXOS_OZONE_WL = "1";
      MOZ_USE_XINPUT2 = "1";
      # https://docs.mesa3d.org/envvars.html#envvar-RADV_PERFTEST
      RADV_PERFTEST = "rt";
    };
  };

  fonts.packages = with pkgs; [
    noto-fonts
    noto-fonts-extra
    noto-fonts-emoji
    symbola
  ];

  programs = {
    dconf.enable = true;
    adb.enable = true;
    java.enable = true;
    fuse.userAllowOther = true;
  };

  gtk.iconCache.enable = true;

  services = {
    udev.extraRules = ''
      ACTION=="add|change", KERNEL=="sd[a-z]", ATTRS{queue/rotational}=="1", RUN+="${pkgs.hdparm}/bin/hdparm -B 254 /dev/%k"
    '';
    fwupd.enable = true;
    pipewire = {
      enable = true;
      systemWide = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse.enable = true;
    };
    yggdrasil = {
      enable = true;
      settings = {
        Peers = [
          "tls://yggno.de:18227"
          "tls://box.paulll.cc:13338"
          "tls://54.37.137.221:11129"
          "tls://pl1.servers.devices.cwinfo.net:11129"
          "tls://193.111.114.28:1443"
          "tls://ygg-ukr.incognet.io:8884"
        ];
      };
      openMulticastPort = true;
      persistentKeys = true;
    };
    hans.clients.digitalocean = {
      server = "kurnevsky.net";
      passwordFile = "/secrets/hans";
      extraConfig = "-d icmp -m 1200";
    };
    iodine.clients.digitalocean = {
      server = "i.kurnevsky.net";
      passwordFile = "/secrets/iodine";
    };
    i2pd = {
      enable = true;
      proto = {
        http.enable = true;
        httpProxy.enable = true;
        socksProxy.enable = true;
        i2cp.enable = true;
      };
      outTunnels = {
        SMTP = {
          address = "127.0.0.1";
          port = 7659;
          destination = "smtp.postman.i2p";
          destinationPort = 25;
        };
        POP3 = {
          address = "127.0.0.1";
          port = 7660;
          destination = "pop.postman.i2p";
          destinationPort = 110;
        };
        IRC = {
          address = "127.0.0.1";
          port = 6668;
          destination = "irc.ilita.i2p";
        };
      };
      yggdrasil.enable = true;
    };
    monero = {
      enable = true;
      extraConfig = ''
        prune-blockchain=1
        pad-transactions=1
        tx-proxy=i2p,127.0.0.1:4447
        tx-proxy=tor,127.0.0.1:9050
      '';
    };
    printing.enable = true;
    resolved.enable = true;
    tor = {
      enable = true;
      client = {
        enable = true;
        transparentProxy.enable = true;
        dns.enable = true;
      };
      settings.Socks5Proxy = "127.0.0.1:1080";
    };
    upower.enable = true;
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
    bloop.install = true;
  };

  virtualisation = {
    docker.rootless = {
      enable = true;
      setSocketVariable = true;
    };
    waydroid.enable = true;
  };

  hardware = {
    bluetooth.enable = true;
    usb-modeswitch.enable = true;
    opengl = {
      enable = true;
      driSupport32Bit = true;
    };
    flipperzero.enable = true;
  };

  systemd = {
    user.services = {
      dbus.wantedBy = [ "default.target" ];
      docker.wantedBy = pkgs.lib.mkForce [ ];
      # It caches java path there.
      bloop.serviceConfig.ExecStartPre =
        "${pkgs.coreutils}/bin/rm -rf %h/.bloop/";
      cloud-mdir-sync = let
        cfg = pkgs.writeText "cms.cfg" ''
          account = Office365_Account(user="ykurneuski@evolution.com")
          CredentialServer("/run/user/1000/cms.sock", accounts=[account], protocols=["IMAP"])
        '';
      in {
        description = "cloud-mdir-sync service";
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        serviceConfig = {
          Restart = "on-failure";
          ExecStart = "${pkgs.cloud-mdir-sync}/bin/cloud-mdir-sync -c ${cfg}";
        };
      };
    };
    services = {
      iodine-digitalocean.wantedBy = pkgs.lib.mkForce [ ];
      hans-digitalocean.wantedBy = pkgs.lib.mkForce [ ];
      i2pd.wantedBy = pkgs.lib.mkForce [ ];
      monero = {
        wantedBy = pkgs.lib.mkForce [ ];
        requires = [ "i2pd.service" "tor.service" ];
      };
      tor.wantedBy = pkgs.lib.mkForce [ ];
      waydroid-container.wantedBy = pkgs.lib.mkForce [ ];
    };
    network = {
      enable = true;
      netdevs = {
        "99-wg0" = {
          netdevConfig = {
            Name = "wg0";
            Kind = "wireguard";
            Description = "WireGuard tunnel wg0";
            MTUBytes = "1280";
          };
          wireguardConfig = {
            PrivateKeyFile = "/secrets/wg/private.key";
            ListenPort = 51871;
          };
          wireguardPeers = [{
            wireguardPeerConfig = {
              PublicKey = "5JHCxIYeZ50k7YJM+kLAbqGW4LAXpI5lycYEWSVxkBE=";
              PresharedKeyFile = "/secrets/wg/preshared.psk";
              AllowedIPs = "0.0.0.0/0, ::/0";
              # Direct connection
              # Endpoint = "kurnevsky.net:51871";
              # Websocat connection
              # Endpoint = "127.0.0.1:42930";
              # Shadowsocks connection
              Endpoint = "127.0.0.1:51870";
              PersistentKeepalive = 25;
            };
          }];
        };
      };
      networks."99-wg0" = {
        name = "wg0";
        routes = [{
          routeConfig = {
            Destination = "192.168.14.0/24";
            Scope = "link";
          };
        }];
      };
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
      commands = [
        "/run/current-system/sw/bin/ip ^netns exec [[:alnum:]]+ sudo -u ww [^-].*$"
      ];
    }];
  };

  users = {
    users = {
      kurnevsky.extraGroups =
        [ "adbusers" "video" "pipewire" "vboxusers" "networkmanager" ];
      ww = {
        uid = 1001;
        isNormalUser = true;
        shell = pkgs.zsh;
        hashedPasswordFile = "/secrets/ww";
        extraGroups = [ "video" "pipewire" ];
      };
      hans.group = "hans";
    };
    groups.hans = { };
  };

  xdg.mime = {
    enable = true;
    defaultApplications = import ./default-applications.nix;
  };

  home-manager = let
    home-config = {
      home.file = {
        ".face.icon".source = ../resources/face.icon;
        ".wallpaper.jpg".source = ../resources/wallpaper.jpg;
        ".config/tox/toxic.conf".source = ./toxic.conf;
      };
      programs = {
        gpg.package = pkgs.gnupg_patched;
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
            vo = "gpu-next";
            target-colorspace-hint = true;
            gpu-api = "vulkan";
          };
          bindings = {
            "Ctrl+n" = ''af toggle "lavfi=[dynaudnorm=f=175:g=25:p=0.75]"'';
          };
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
        git.enable = true;
        firefox = {
          enable = true;
          package = pkgs.firefox;
          profiles.default = {
            settings = import ./firefox/firefox.nix;
            userChrome = builtins.readFile ./firefox/userChrome.css;
            extensions = with pkgs.nur.repos.rycee.firefox-addons; [
              darkreader
              decentraleyes
              plasma-integration
              privacy-badger
              sponsorblock
              ublock-origin
              brandon1024-find
              passff
              i-dont-care-about-cookies
            ];
          };
        };
        mbsync = { enable = true; };
      };
      accounts.email.accounts = {
        gmail = {
          mbsync = {
            enable = true;
            patterns = [ "*" "![Gmail]/All Mail" ];
            create = "both";
            expunge = "both";
            extraConfig.account.AuthMech = "PLAIN";
          };
          primary = true;
          maildir.path = "gmail";
          userName = "kurnevsky@gmail.com";
          imap.host = "imap.gmail.com";
          passwordCommand =
            "${pkgs.pass}/bin/pass show web/google.com | grep Isync | cut -d ' ' -f 2";
        };
        yandex = {
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            extraConfig.account.AuthMech = "PLAIN";
          };
          maildir.path = "yandex";
          userName = "kurnevsky";
          imap.host = "imap.ya.ru";
          passwordCommand =
            "${pkgs.pass}/bin/pass show web/yandex.ru | head -n 1";
        };
        evolution = let user = "ykurneuski@evolution.com";
        in {
          mbsync = {
            enable = true;
            create = "both";
            expunge = "both";
            extraConfig.account.AuthMech = "XOAUTH2";
          };
          maildir.path = "evolution";
          userName = user;
          imap.host = "outlook.office365.com";
          passwordCommand =
            "${pkgs.cloud-mdir-sync}/bin/cms-oauth --cms_sock=$XDG_RUNTIME_DIR/cms.sock --proto=IMAP --user ${user} --output=token";
        };
      };
      services.gpg-agent = {
        enable = true;
        pinentryFlavor = "qt";
      };
      # To make sure that it's not overridden by WM
      xdg.mimeApps.enable = true;
    };
  in {
    users = {
      kurnevsky = home-config;
      ww = home-config;
    };
  };
}
