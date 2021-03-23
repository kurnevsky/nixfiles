{ config, pkgs, ... }:

{
  imports = [
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
  boot.kernelParams = [ "zswap.enabled=1" ];

  fileSystems."/".options = [ "noatime" "nodiratime" "compress=zstd:3" ];

  nix = {
    package = pkgs.nixFlakes;
    autoOptimiseStore = true;
    extraOptions = "experimental-features = nix-command flakes";
    binaryCaches = [ "https://cachix.cachix.org" ];
    binaryCachePublicKeys =
      [ "cachix.cachix.org-1:eWNHQldwUO7G2VkjpnjDbWwy4KQ/HNxht7H4SSoMckM=" ];
  };

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
    mpv
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
    tor-browser-bundle-bin
    torsocks
    toxic
    trayer
    unzipNatspec
    vdpauinfo
    viu
    vlc
    volumeicon
    vscodium
    websocat
    wesnoth
    wget
    wineWowPackages.staging
    winetricks
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
  ];

  programs.gnupg.agent.enable = true;

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    autosuggestions.enable = true;
    syntaxHighlighting.enable = true;
  };

  programs.adb.enable = true;

  services = {
    haveged.enable = true;
    i2p.enable = true;
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
      # Enable touchpad support.
      # libinput.enable = true;
      layout = "us,ru";
      xkbOptions = "grp:caps_toggle,grp_led:caps,terminate:ctrl_alt_bksp";
      displayManager.startx.enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = pkgs: with pkgs; [ dbus regex-compat taffybar ];
        # config = pkgs.builtins.readFile "/etc/xmonad.hs";
      };
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
  systemd.services.i2p.wantedBy = pkgs.lib.mkForce [ ];
  systemd.services.monero.wantedBy = pkgs.lib.mkForce [ ];
  systemd.services.tor.wantedBy = pkgs.lib.mkForce [ ];

  # Enable pam_systemd module to set dbus environment variable.
  security.pam.services.login.startSession = true;
  security.unprivilegedUsernsClone = true;

  users.users.kurnevsky = {
    isNormalUser = true;
    extraGroups = [ "wheel" "adbusers" ];
    shell = pkgs.zsh;
  };

  nixpkgs.overlays = [
    (self: super: {
      uutils-coreutils = super.uutils-coreutils.override { prefix = null; };
    })
    (self: super: rec {
      zipNatspec = super.zip.override { enableNLS = true; };
      unzipNatspec = super.unzip.override { enableNLS = true; };
      mc = super.mc.override {
        zip = zipNatspec;
        unzip = unzipNatspec;
      };
    })
    (self: super: {
      tesseract =
        super.tesseract.override { enableLanguages = [ "eng" "rus" ]; };
    })
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?
}
