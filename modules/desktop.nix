{
  inputs,
  config,
  pkgs,
  ...
}:

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
    loader = {
      grub.memtest86.enable = true;
      systemd-boot = {
        memtest86.enable = true;
        consoleMode = "max";
      };
    };
    initrd.systemd.enable = true;
    plymouth.enable = true;
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
      allowedTCPPorts = [
        # Yggdrasil multicast
        17606
      ];
      trustedInterfaces = [
        "wg0"
        "icmp"
        "dns0"
      ];
    };
    wireguard.enable = true;
  };

  i18n.supportedLocales = [
    "C.UTF-8/UTF-8"
    "en_US.UTF-8/UTF-8"
    "ru_RU.UTF-8/UTF-8"
  ];

  console = {
    packages = with pkgs; [ terminus_font ];
    font = "${pkgs.terminus_font}/share/consolefonts/ter-u16n.psf.gz";
    keyMap = "ru";
    earlySetup = true;
  };

  environment = {
    systemPackages =
      with pkgs;
      with sandboxed;
      [
        (wrapOBS {
          plugins = with obs-studio-plugins; [
            obs-gstreamer
            obs-backgroundremoval
          ];
        })
        (pkgs.pkgsCross.mingw32.callPackage ./vkd3d-proton.nix { })
        (pkgs.pkgsCross.mingwW64.callPackage ./vkd3d-proton.nix { })
        anki
        blender
        calibre
        claws-mail
        cloud-mdir-sync
        (czkawka.wrapper.override {
          extraPackages = [ ffmpeg-full ];
        })
        dosbox
        eiskaltdcpp
        gimp3-with-plugins
        scrcpy
        gnome-themes-extra
        adwaita-icon-theme
        gparted
        inkscape
        keepassxc
        languagetool
        lapce
        libreoffice-fresh
        mc
        pavucontrol
        qbittorrent
        qemu
        radare2
        tesseract
        tigervnc
        ubpm
        wineWowPackages.stagingFull
        winetricks
        dxvk.out
        zathura
        # CLI utils
        (aspellWithDicts (
          dicts: with dicts; [
            en
            ru
          ]
        ))
        (hunspell.withDicts (
          dicts: with dicts; [
            en_US
            ru_RU
          ]
        ))
        aircrack-ng
        ansible
        ansible-lint
        appimage-run
        aria2
        arp-scan
        barcode
        bat
        bc
        bind
        bindfs
        binutils
        bottom
        brightnessctl
        btrfs-progs
        bubblewrap
        clinfo
        cuetools
        curlHTTP3
        davfs2
        dbus
        dmidecode
        docker-compose
        dosfstools
        dsniff
        e2fsprogs
        efibootmgr
        exfat
        exploitdb
        extundelete
        eza
        fclones
        fd
        fdroidcl
        ffmpeg-full
        flac
        fuseiso
        gdb
        gnupg
        gpclient
        graphicsmagick
        graphicsmagick-imagemagick-compat
        hdparm
        (pkgs.symlinkJoin {
          name = "jira-cli";
          paths = [
            (pkgs.writeShellScriptBin "jira" ''
              ${pkgs.coreutils}/bin/env \
                JIRA_AUTH_TYPE=bearer \
                JIRA_API_TOKEN="$(pass auth-source/jira)" \
                ${jira-cli-go}/bin/jira "$@"
            '')
            jira-cli-go
          ];
        })
        inetutils
        innoextract
        iotop
        isync
        jq
        keyutils
        kubectl
        kubelogin-oidc
        libnotify
        libva-utils
        libxml2
        lm_sensors
        lsd
        lshw
        mesa-demos
        metasploit
        mu
        nettools
        nmap
        openssl
        pandoc
        parallel
        pass
        pciutils
        pgpdump
        playerctl
        psmisc
        pulseaudio
        qrencode
        rage
        reptyr
        rclone
        ripgrep
        rkdeveloptool-pine64
        minicom
        rsync
        shntool
        skim
        smartmontools
        sshfs-fuse
        tealdeer
        tmux
        torsocks
        translate-shell
        umu-launcher
        unixtools.xxd
        usbutils
        v4l-utils
        vdpauinfo
        viu
        vorbis-tools
        vulkan-tools
        wavpack
        waypipe
        websocat
        wget
        wirelesstools
        xmlstarlet
        yt-dlp
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
        # RSS
        newsboat
        rssguard
        # Messengers
        gajim
        telegram-desktop
        ## Matrix
        element-desktop
        ## Tox
        toxic
        qtox
        # Multimedia
        ## Image
        feh
        imv
        oculante
        ## Video
        mpv
        vlc
        ## Audio
        deadbeef-with-plugins
        mpc_cli
        ncmpc
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
        feather
        # Audio
        easyeffects
        helvum
        qpwgraph
        sox
        # Languages
        (agda.withPackages (pkgs: with pkgs; [ standard-library ]))
        (pkgs.writeShellScriptBin "prettier" ''
          ${pkgs.nodePackages.prettier}/bin/prettier --plugin "${
            pkgs.callPackage ./prettier-plugin-toml.nix {
              nodeEnv = pkgs.callPackage "${pkgs.path}/pkgs/development/node-packages/node-env.nix" { };
            }
          }/lib/node_modules/prettier-plugin-toml/lib/index.cjs" "$@"
        '')
        astyle
        groovy
        mono
        nodePackages.mermaid-cli
        perl
        python3
        (pkgs.writeShellScriptBin "famulus" ''
          ${pkgs.coreutils}/bin/env \
            MISTRAL_API_KEY="$(pass auth-source/mistral)" \
            OPENAI_API_KEY="$(pass auth-source/groq)" \
            ${callPackage ./famulus.nix { }}/bin/famulus "$@"
        '')
        ## C/C++
        clang
        clang-tools
        gcc
        ## Shell
        nodePackages.bash-language-server
        shellcheck
        shfmt
        ## Haskell
        cabal-install
        haskellPackages.cabal-fmt
        ghc
        haskell-language-server
        hlint
        ## PureScript
        nodePackages.purescript-language-server
        nodePackages.purs-tidy
        purescript
        spago-unstable
        purs-backend-es
        ## Rust
        (fenix.combine [
          (fenix.stable.withComponents [
            "cargo"
            "clippy"
            "rust-src"
            "rustc"
            "rustfmt"
          ])
          fenix.targets.wasm32-unknown-unknown.stable.rust-std
          fenix.targets.wasm32-wasip2.stable.rust-std
        ])
        rust-analyzer
        ## Java
        jdt-language-server
        maven
        ## Scala
        coursier
        sbt
        scala-next
        metals
        ## JavaScript
        nodejs
        esbuild
        ## Lean
        lean4
        ## Idris
        idris2Packages.idris2Lsp
        idris2
        ## Nix
        alejandra
        deadnix
        nixfmt-rfc-style
        nil
        nix-diff
        nixpkgs-review
        statix
        nix-output-monitor
        ## Math
        maxima
        wxmaxima
        octave
        (rWrapper.override {
          packages = with rPackages; [
            styler
            ggplot2
          ];
        })
        ## Markdown
        nodePackages.markdownlint-cli
        marksman
        ## TypeScript
        nodePackages.typescript-language-server
        ## Dhall
        dhall
        dhall-lsp-server
        # VCS
        mercurial
        pijul
        subversion
      ];

    variables = {
      EMAIL = "kurnevsky@gmail.com";
      ALTERNATE_EDITOR = "nano";
      VIEWER = "less";
      # Fixes aspell when it's used as build time dependency,
      # e.g. enchant in emacs
      ASPELL_CONF = "dict-dir /run/current-system/sw/lib/aspell";
      RCLONE_PASSWORD_COMMAND = "pass rclone";
    };

    sessionVariables.NIXOS_OZONE_WL = "1";
  };

  fonts = {
    packages =
      with pkgs;
      let
        iosevka-custom = callPackage ./iosevka.nix { };
        iosevka-term = iosevka-custom "Term" false;
        iosevka-normal = iosevka-custom "Normal" true;
        iosevka-quasi-proportional = iosevka-custom "Quasi-Proportional" true;
      in
      [
        iosevka-term
        iosevka-normal
        iosevka-quasi-proportional
        (callPackage ./nerd-font-patch.nix { } iosevka-term)
        (callPackage ./nerd-font-patch.nix { } iosevka-normal)
        (callPackage ./nerd-font-patch.nix { } iosevka-quasi-proportional)
        nerd-fonts.symbols-only
        noto-fonts
        noto-fonts-color-emoji
        symbola
      ];
    fontconfig = {
      subpixel.rgba = "rgb";
      localConf = ''
        <alias>
          <family>monospace</family>
          <prefer><family>Symbols Nerd Font</family></prefer>
        </alias>
      '';
    };
  };

  programs = {
    nix-ld.enable = true;
    dconf.enable = true;
    adb.enable = true;
    java.enable = true;
    fuse.userAllowOther = true;
    chromium.enable = true;
  };

  gtk.iconCache.enable = true;

  services = {
    solaar.enable = true;
    envfs.enable = true;
    dbus.implementation = "broker";
    avahi = {
      enable = true;
      denyInterfaces = [ "tun0" ];
    };
    udev.extraRules = ''
      ACTION=="add|change", KERNEL=="sd[a-z]", ATTRS{queue/rotational}=="1", RUN+="${pkgs.hdparm}/bin/hdparm -B 254 /dev/%k"
    '';
    fwupd.enable = true;
    automatic-timezoned.enable = true;
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
          "tls://ygg.mkg20001.io:443"
        ];
        MulticastInterfaces = [
          {
            Regex = "(en|wl).*";
            Beacon = true;
            Listen = true;
            Port = 17606;
          }
        ];
      };
      openMulticastPort = true;
      persistentKeys = true;
    };
    hans.clients.digitalocean = {
      server = "kropki.org";
      passwordFile = config.age.secrets.hans.path or "/secrets/hans";
      extraConfig = "-d icmp -m 1200";
    };
    iodine.clients.digitalocean = {
      server = "i.kropki.org";
      passwordFile = config.age.secrets.iodine.path or "/secrets/iodine";
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
      settings = {
        Socks5Proxy = "127.0.0.1:1080";
        ControlPort = [ { port = 9051; } ];
        CookieAuthentication = true;
        CookieAuthFileGroupReadable = true;
      };
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
    libvirtd = {
      enable = true;
      qemu.runAsRoot = false;
    };
  };

  hardware = {
    bluetooth.enable = true;
    usb-modeswitch.enable = true;
    graphics = {
      enable = true;
      enable32Bit = true;
    };
    flipperzero.enable = true;
  };

  systemd = {
    user.services = {
      dbus.wantedBy = [ "default.target" ];
      docker.wantedBy = pkgs.lib.mkForce [ ];
      # It caches java path there.
      bloop.serviceConfig.ExecStartPre = "${pkgs.coreutils}/bin/rm -rf %h/.bloop/";
      cloud-mdir-sync =
        let
          cfg = pkgs.writeText "cms.cfg" ''
            account = Office365_Account(user="ykurneuski@evolution.com")
            CredentialServer("/run/user/1000/cms.sock", accounts=[account], protocols=["IMAP"])
          '';
        in
        {
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
        requires = [
          "i2pd.service"
          "tor.service"
        ];
      };
      tor.wantedBy = pkgs.lib.mkForce [ ];
      waydroid-container.wantedBy = pkgs.lib.mkForce [ ];
      libvirtd.wantedBy = pkgs.lib.mkForce [ ];
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
            PrivateKeyFile = config.age.secrets.wg-private.path or "/secrets/wg/private.key";
            ListenPort = 51871;
          };
          wireguardPeers = [
            {
              PublicKey = "5JHCxIYeZ50k7YJM+kLAbqGW4LAXpI5lycYEWSVxkBE=";
              PresharedKeyFile = config.age.secrets.wg-preshared.path or "/secrets/wg/preshared.psk";
              AllowedIPs = "0.0.0.0/0, ::/0";
              # Direct connection
              # Endpoint = "kropki.org:51871";
              # Websocat connection
              # Endpoint = "127.0.0.1:42930";
              # Shadowsocks connection
              Endpoint = "127.0.0.1:51870";
              PersistentKeepalive = 25;
            }
          ];
        };
      };
      networks."99-wg0" = {
        name = "wg0";
        routes = [
          {
            Destination = "192.168.14.0/24";
            Scope = "link";
          }
        ];
      };
    };
  };

  security = {
    # Enable pam_systemd module to set dbus environment variable.
    pam.services.login.startSession = true;
    unprivilegedUsernsClone = true;
    rtkit.enable = true;
    sudo.extraRules = [
      {
        runAs = "root";
        users = [ "ww" ];
        commands = [
          "/run/current-system/sw/bin/ip ^netns exec [[:alnum:]]+ sudo -u ww [^-].*$"
        ];
      }
    ];
  };

  users = {
    users = {
      kurnevsky.extraGroups = [
        "adbusers"
        "libvirtd"
        "video"
        "pipewire"
        "vboxusers"
        "networkmanager"
        "tor"
      ];
      ww = {
        uid = 1001;
        isNormalUser = true;
        shell = pkgs.zsh;
        hashedPasswordFile = config.age.secrets.ww.path or "/secrets/ww";
        extraGroups = [
          "video"
          "pipewire"
          "tor"
        ];
      };
      hans.group = "hans";
    };
    groups.hans = { };
  };

  xdg.mime = {
    enable = true;
    defaultApplications = import ./default-applications.nix;
  };

  home-manager =
    let
      home-config = {
        home.file = {
          ".config/tox/toxic.conf".source = ./toxic.conf;
          ".config/mpv/scripts/next_unexpanded.lua".source = ./next_unexpanded.lua;
        };
        programs = {
          feh = {
            enable = true;
            package = pkgs.sandboxed.feh;
            buttons = {
              zoom_in = "C-4";
              zoom_out = "C-5";
            };
          };
          mpv = {
            enable = true;
            package = pkgs.sandboxed.mpv;
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
                normal.family = "IosevkaTerm Nerd Font";
                size = 12;
              };
              colors =
                with config.scheme;
                let
                  x = c: "0x${c}";
                in
                {
                  draw_bold_text_with_bright_colors = false;
                  primary = {
                    background = x base00;
                    foreground = x base05;
                  };
                  cursor = {
                    text = x base00;
                    cursor = x base05;
                  };
                  normal = {
                    black = x base01;
                    red = x base08;
                    green = x base0B;
                    yellow = x base09;
                    blue = x base0D;
                    magenta = x base0E;
                    cyan = x base0C;
                    white = x base06;
                  };
                  bright = {
                    black = x base02;
                    red = x base12;
                    green = x base14;
                    yellow = x base13;
                    blue = x base16;
                    magenta = x base17;
                    cyan = x base15;
                    white = x base07;
                  };
                };
              cursor.style = "Beam";
              general.live_config_reload = false;
              hints.enabled = [
                {
                  regex = ''(ipfs:|ipns:|magnet:|mailto:|gemini:|gopher:|https:|http:|news:|file:|git:|ssh:|ftp:)[^\u0000-\u001F\u007F-\u009F<>"\\s{-}\\^⟨⟩`]+'';
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
                }
              ];
              keyboard.bindings = [
                {
                  key = "Insert";
                  mods = "Control";
                  action = "Copy";
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
            package = pkgs.sandboxed.firefox;
            profiles.default = {
              settings = import ./firefox/firefox.nix;
              userChrome = builtins.readFile ./firefox/userChrome.css;
              extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
                darkreader
                decentraleyes
                plasma-integration
                privacy-badger
                sponsorblock
                ublock-origin
                brandon1024-find
                keepassxc-browser
                istilldontcareaboutcookies
                refined-github
                canvasblocker
              ];
            };
          };
          chromium = {
            enable = true;
            package = pkgs.sandboxed.ungoogled-chromium;
            extensions = builtins.attrValues pkgs.chromium-extensions;
          };
        };
        services.gpg-agent = {
          enable = true;
          pinentry.package = pkgs.pinentry-qt;
        };
        # To make sure that it's not overridden by WM
        xdg.mimeApps.enable = true;
      };
    in
    {
      users = {
        kurnevsky = pkgs.lib.recursiveUpdate home-config {
          home.file = {
            ".face.icon".source = ../resources/face.icon;
            ".wallpaper.jpg".source = ../resources/wallpaper.jpg;
          };
          programs = {
            mbsync = {
              enable = true;
              package = pkgs.sandboxed.isync;
            };
            vdirsyncer = {
              enable = true;
              package = pkgs.sandboxed.vdirsyncer;
            };
            khal = {
              enable = true;
              locale = {
                timeformat = "%H:%M";
                dateformat = "%Y-%m-%d";
                longdateformat = "%Y-%m-%d %a";
                datetimeformat = "%Y-%m-%d %H:%M";
                longdatetimeformat = "%Y-%m-%d %H:%M";
              };
            };
          };
          accounts = {
            email.accounts = {
              gmail = {
                mbsync = {
                  enable = true;
                  patterns = [
                    "*"
                    "![Gmail]/All Mail"
                  ];
                  create = "both";
                  expunge = "both";
                  extraConfig.account.AuthMech = "PLAIN";
                };
                primary = true;
                maildir.path = "gmail";
                userName = "kurnevsky@gmail.com";
                imap.host = "imap.gmail.com";
                passwordCommand = "${pkgs.pass}/bin/pass show web/google.com | grep Isync | cut -d ' ' -f 2";
              };
              evolution =
                let
                  user = "ykurneuski@evolution.com";
                in
                {
                  mbsync = {
                    enable = true;
                    create = "both";
                    expunge = "both";
                    extraConfig.account.AuthMech = "XOAUTH2";
                  };
                  maildir.path = "evolution";
                  userName = user;
                  imap.host = "outlook.office365.com";
                  passwordCommand = "${pkgs.cloud-mdir-sync}/bin/cms-oauth --cms_sock=$XDG_RUNTIME_DIR/cms.sock --proto=IMAP --user ${user} --output=token";
                };
            };
            calendar = {
              basePath = "Calendar";
              accounts = {
                evo = {
                  khal = {
                    enable = true;
                    readOnly = true;
                  };
                  vdirsyncer = {
                    enable = true;
                    urlCommand = [
                      "pass"
                      "evo/outlook-ical"
                    ];
                  };
                  remote.type = "http";
                  local = {
                    type = "filesystem";
                    fileExt = ".ics";
                  };
                };
                google = {
                  khal = {
                    enable = true;
                    readOnly = true;
                  };
                  vdirsyncer = {
                    enable = true;
                    urlCommand = [
                      "pass"
                      "auth-source/google-calendar"
                    ];
                  };
                  remote.type = "http";
                  local = {
                    type = "filesystem";
                    fileExt = ".ics";
                  };
                };
              };
            };
          };
        };
        ww = home-config;
      };
    };
}
