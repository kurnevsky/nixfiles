{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./shadowsocks.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  # boot.loader.grub.efiSupport = true;
  # boot.loader.grub.efiInstallAsRemovable = true;
  # boot.loader.efi.efiSysMountPoint = "/boot/efi";
  # Define on which hard drive to install Grub.
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.luks.devices.root.allowDiscards = true;
  boot.kernelPackages = pkgs.linuxPackages_hardened;
  boot.extraModulePackages = with config.boot.kernelPackages; [
    acpi_call
    v4l2loopback
  ];
  boot.kernelParams = [
    "zswap.enabled=1"
  ];

  fileSystems."/".options = [
    "noatime"
    "nodiratime"
    "compress=zstd:3"
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
    binutils
    btrfs-progs
    bubblewrap
    cabal-install
    calibre
    cargo
    cataclysm-dda
    chromium
    clinfo
    coursier
    davfs2
    deadbeef-with-plugins
    docker-compose
    dosbox
    dosfstools
    dunst
    e2fsprogs
    editorconfig-core-c
    eiskaltdcpp
    element-desktop
    emacs
    exa
    exfat-utils
    extundelete
    fbreader
    fd
    fdupes
    feh
    ffmpeg
    firefox
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
    imv
    inkscape
    innoextract
    iodine
    iotop
    isync
    jetbrains.idea-community
    jq
    kafkacat
    languagetool
    libreoffice-fresh
    lsd
    lshw
    maim
    maxima
    mc
    mercurial
    mesa-demos
    metals
    metasploit
    monero # TODO: use service?
    mono
    motion
    mpd
    mpv
    mu
    nettools
    networkmanagerapplet
    newsboat
    nix-diff
    nmap
    nodePackages.bash-language-server
    nodePackages.prettier
    ntfs3g
    numlockx
    octave
    openconnect
    openjdk
    openmw
    p7zip
    pandoc # TODO: it should depend on texlive
    parallel
    parcellite
    passff-host
    pavucontrol
    perl
    picom
    pidgin-with-plugins
    psmisc # for killall
    qbittorrent
    qemu
    qrencode
    qt5ct
    qtox
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
    taffybar
    tdesktop
    telegram-purple
    tesseract
    texlive.combined.scheme-basic
    tigervnc
    tinc
    tldr
    tmux
    tor # TODO: use service?
    tor-browser-bundle-bin
    torsocks
    toxic
    trayer
    unzipNatspec
    vlc
    volumeicon
    vscodium
    websocat
    wget
    wireguard-tools
    wmctrl
    wxmaxima
    xcalib
    xdotool
    xmlstarlet
    xmobar
    xorg.xbacklight
    xscreensaver
    xterm
    you-get
    youtube-dl
    zathura
    zbar
    # findimagedupes
    # tuntox
    # uutils-coreutils
    # vagrant
    # vdpauinfo
    # veloren
    # virtualbox
    # wesnoth
    # wine
    # winetricks
    # tensorflow
    # sane xsane
  ];

  fonts.fonts = with pkgs; [
    (nerdfonts.override {
      fonts = [ "Hack" ];
    })
    noto-fonts
    noto-fonts-extra
    noto-fonts-emoji
  ];

  programs.gnupg.agent.enable = true;

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
  };

  programs.adb.enable = true;

  services.openssh = {
    enable = true;
    forwardX11 = true;
  };
  services.timesyncd.enable = true;
  services.resolved.enable = true;
  services.haveged.enable = true;
  services.i2p.enable = true;

  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 ];
    allowedUDPPorts = [ 33445 ];
  };

  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;
  };

  services.printing.enable = true;

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
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

  services.tlp.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "us,ru";
  # services.xserver.xkbOptions = "eurosign:e";

  # Enable touchpad support.
  # services.xserver.libinput.enable = true;

  services.xserver.displayManager.startx.enable = true;
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    extraPackages = pkgs: with pkgs; [
      dbus
      regex-compat
      taffybar
    ];
    # config = pkgs.builtins.readFile "/etc/xmonad.hs";
  };

  systemd.user.services.dbus.wantedBy = [ "default.target" ];
  systemd.services.i2p.wantedBy = pkgs.lib.mkForce [ ];

  # Enable pam_systemd module to set dbus environment variable.
  security.pam.services.login.startSession = true;
  security.unprivilegedUsernsClone = true;

  users.users.kurnevsky = {
    isNormalUser = true;
    extraGroups = [
      "wheel"
      "adbusers"
    ];
    shell = pkgs.zsh;
  };

  nixpkgs.overlays = [
    ( self: super: {
      uutils-coreutils = super.uutils-coreutils.override {
        prefix = null;
      };
    })
    ( self: super: rec {
      zipNatspec = super.zip.override {
        enableNLS = true;
      };
      unzipNatspec = super.unzip.override {
        enableNLS = true;
      };
      mc = super.mc.override {
        zip = zipNatspec;
        unzip = unzipNatspec;
      };
    })
    ( self: super: {
      tesseract = super.tesseract.override {
        enableLanguages = [
          "eng"
          "rus"
        ];
      };
    })
    ( self: super: {
      motion = super.motion.overrideAttrs (oldAttrs: {
        patches = (oldAttrs.patches or []) ++ [ (pkgs.fetchpatch {
          name = "first-picture-output.patch";
          url = "https://patch-diff.githubusercontent.com/raw/Motion-Project/motion/pull/1136.patch";
          sha256 = "0g48j2xlzg3fifzqwy2llx1bb8wy685698bn0fjgz62m6fdpwr4j";
        }) ];
      });
    })
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
}
