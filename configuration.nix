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
    alacritty
    bat
    bubblewrap
    chromium
    deadbeef-with-plugins
    dunst
    element-desktop
    emacs
    exa
    fd
    feh
    ffmpeg
    firefox
    git
    gitAndTools.delta
    gnome-themes-extra
    gnome3.adwaita-icon-theme
    hans
    hicolor-icon-theme # contains deadbeef icon
    imv
    iodine
    jq
    libreoffice-fresh
    lsd
    maim
    mc
    mesa-demos
    motion
    mpv
    mu
    networkmanagerapplet
    numlockx
    p7zip
    parcellite
    pavucontrol
    picom
    pidgin
    psmisc # for killall
    qbittorrent
    qt5ct
    qtox
    ripgrep
    ripgrep-all
    skim
    sourceHighlight
    starship
    tdesktop
    telegram-purple
    tigervnc
    tmux
    tor
    tor-browser-bundle-bin
    trayer
    vlc
    volumeicon
    wget
    wmctrl
    xcalib
    xmobar
    xorg.xbacklight
    xscreensaver
    xterm
    zathura
    # uutils-coreutils
    # shadowsocks-v2ray-plugin
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

  services.openssh = {
    enable = true;
    forwardX11 = true;
  };
  services.timesyncd.enable = true;
  services.resolved.enable = true;

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 22 ];
  networking.firewall.allowedUDPPorts = [ 33445 ];

  # Enable CUPS to print documents.
  # services.printing.enable = true;

  hardware.pulseaudio.enable = true;

  hardware.opengl = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      vaapiVdpau
      libvdpau-va-gl
    ];
  };

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
    ];
    # config = pkgs.builtins.readFile "/etc/xmonad.hs";
  };

  systemd.user.services.dbus.wantedBy = [ "default.target" ];

  # Enable pam_systemd module to set dbus environment variable.
  security.pam.services.login.startSession = true;

  users.users.kurnevsky = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    shell = pkgs.zsh;
  };

  nixpkgs.overlays = [
    ( self: super: {
      uutils-coreutils = super.uutils-coreutils.override {
        prefix = null;
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
