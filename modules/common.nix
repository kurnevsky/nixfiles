{
  config,
  pkgs,
  lib,
  ...
}:

let
  keys = [
    # PC
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOXwha6y5rgUyl0yVm9YNZNqxZPZ4YG9JgoAVw5sse2P kurnevsky@gmail.com"
    # Dell
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHqAQVhK6ozN935PQhAGv2UTNcuatFtJ6neguBA7FOvQ kurnevsky@gmail.com"
    # Evo
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGsrYtfECDepESxUHHWdQI2BrqVRanY790pyvgYYI3Rm ykurneuski@evolution.com"
    # Android
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJRMyUWZM8dmi9iVFRgpZRrZiKaGfiHvt6iOPMFWb1LW kurnevsky@gmail.com"
  ];
in
{
  boot.kernel.sysctl."net.ipv4.tcp_fastopen" = 3;

  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "symbola"
      "unrar"
      "7zz"
      "p7zip"
      "vagrant"
    ];

  nix = {
    settings = {
      auto-optimise-store = true;
      substituters = [
        "https://nix-community.cachix.org"
        "https://nix-on-droid.cachix.org"
        "https://pinenote-packages.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
        "pinenote-packages.cachix.org-1:kikxnRWwjP5M1jWa31XlRqEkKFC4y8z+GlEtk2hCrII="
        # local keys
        "pc:NVXsu6EBpoB+a+W2ZTkpTWD9Vj5nJtjYsAfe8zTzDKE="
        "evo:aBUuIOLObUPUBWBIW0XGyMZW2Wor5z3ZNtaRynZ35UY="
        "digitalocean:jFnutzjWmSNY5q/frkSkijlCh8GfdNa1Mpm5Y0N15sQ="
      ];
      trusted-users = [ "nix-ssh" ];
    };
    extraOptions = ''
      !include ${config.age.secrets.github.path or "/secrets/github"}
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
      secret-key-files = ${config.age.secrets.store.path or "/secrets/store"}
    '';
    sshServe = {
      enable = true;
      write = true;
      inherit keys;
    };
  };

  environment = {
    systemPackages = with pkgs; [
      git
      lsof
      mc
      sourceHighlight
    ];
    shellAliases = {
      ls = "ls --color=auto";
      grep = "grep --color=auto";
      su = "sudo su";
    };
  };

  networking.hosts = import ./hosts.nix;

  programs = {
    bash.interactiveShellInit = ''
      # Set cursor type to steady bar
      echo -e -n "\x1b[\x36 q"
    '';
    zsh = {
      enable = true;
      enableCompletion = true;
      # will be enabled in interactiveShellInit differently
      enableGlobalCompInit = false;
      autosuggestions = {
        enable = true;
        highlightStyle = "fg=${config.scheme.withHashtag.base02}";
        extraConfig = {
          ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE = "10";
        };
      };
      syntaxHighlighting.enable = true;
      # Override default prompt as it doesn't work good in Midnigt Commander
      promptInit = "PROMPT='[%n@%m %~]$ '";
      histSize = 25000;
      histFile = "~/.histfile";
      setOptions = import ./zsh-options.nix;
      shellAliases = {
        zmv = "noglob zmv -W";
        calc = "noglob zcalc -e";
      };
      interactiveShellInit = builtins.readFile ./interactive-init.zsh;
    };
    less = {
      enable = true;
      lessopen = "| ${pkgs.sourceHighlight}/bin/src-hilite-lesspipe.sh %s";
      envVariables.LESS = " -R ";
    };
    # Replaced by nix-index
    command-not-found.enable = false;
  };

  users = {
    mutableUsers = false;
    motd = import ./motd.nix config.networking.hostName;
    users = {
      # To get hash use:
      # openssl passwd -6 password
      root = {
        shell = pkgs.zsh;
        hashedPassword = "!";
        openssh.authorizedKeys.keys = keys;
      };
      kurnevsky = {
        uid = 1000;
        description = "Evgeny Kurnevsky";
        isNormalUser = true;
        extraGroups = [ "wheel" ];
        shell = pkgs.zsh;
        hashedPasswordFile = config.age.secrets.kurnevsky.path or "/secrets/kurnevsky";
        openssh.authorizedKeys.keys = keys;
      };
    };
  };

  services = {
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };
      extraConfig = "PermitTunnel yes";
    };
    ntpd-rs.enable = true;
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };
}
