{ config, pkgs, lib, ... }:

let
  keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOXwha6y5rgUyl0yVm9YNZNqxZPZ4YG9JgoAVw5sse2P kurnevsky@gmail.com"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCnphYFSCOiMmmvpktspcCc+Qkg8GArzaB3aSxuIgUOQzS/uk+shTSbnHsP/ABvtGPWz3Fc0XJzRkDVQpYaSdpdMWXR7RgxeZ0GzB4954mV+e3wD8qN+4zlQY+g/ablv9HEPnjOrkbwXGERnj/4DVBJQhcrBB6GKIWk7YGOek1Bp70CUp9jdaLhnuh6qnvIRb/CDR9aIDZoQjt9O4DcUJYRsQOdS7QgxzRG8loiy6ur82GarLVR1aQAq8dB46eDZDBS91WxrzrWtk1T7UBg7MKNmQdyVw5aqh6V6s+fpOn+KmjajD0DOIspDNmFe3+TkPEqGFJGMTrqCrWSqtxyhPqJ kurnevsky@gmail.com"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDDPmgbv8HY8cevVCgZPJw+4WQzXxmV8RU0r0owdFAmcMFHc0SLeFtWvZ2fV6LkerPKHzK1uWTwPq1RhqMDVlhzuaDjtgLlNX4VJ84aHzjv62+6gaAbUzEkIwF3nigVI8MECW7r1Sk38yI42VaGn8Qa2ThKwdbqcskeh8eD0TyvVSNe46vz4AnMR/gw2bkkGIIUTWF6MP8/uMbxdErLUSZPaoflfO3RpMQPomNigrgwDxptisY2nWhTSskOu+RVj17yBIQIH0d4EpiezRniQ1YeI47LSj7I01e/zy1HyyEm1S/mKe+uaHDIlcGWllWXam9AKC5atyUiH9lbj0c1vUe9WtP0dk8Zf2qgJwkB0DZAhehVbycw4rP4omUisI/rZjUxXOFk2R/O5asxbtIWsLjAJIW8g6uf9e6T0+5piAuyF3fd3zy4ZIj5/G2EAsywxxB4Jec5kKCHOy4E6tFgF2jtLAgTk4dij/dZVvZsUWYAxBdZjQ7yUIHVCNUU2Br4+NvtyoW7/2JH8EQP+agPCuUVMF0SdWUxhXfDbojEAO9y71D2PiDZwyFAPY15e0hMI80r1A6bZxRiBeufUxnimeGSuBqzUhiBBjGgT3Cm0amJ5ZGSggqss+txVEn2Ntgbi9SZ9X4BiQdE6zylIsFpSVEZ8KYIySxkK2ElQp7XqCaqDw== ykurneuski@evolution.com"
  ];
in {
  boot.kernel.sysctl."net.ipv4.tcp_fastopen" = 3;

  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "symbola" "unrar" "p7zip" "vagrant" ];

  nix = {
    settings = {
      auto-optimise-store = true;
      substituters = [
        "https://nix-community.cachix.org"
        "https://nix-on-droid.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "nix-on-droid.cachix.org-1:56snoMJTXmDRC1Ei24CmKoUqvHJ9XCp+nidK7qkMQrU="
        # local keys
        "pc:NVXsu6EBpoB+a+W2ZTkpTWD9Vj5nJtjYsAfe8zTzDKE="
        "evo:aBUuIOLObUPUBWBIW0XGyMZW2Wor5z3ZNtaRynZ35UY="
        "digitalocean:jFnutzjWmSNY5q/frkSkijlCh8GfdNa1Mpm5Y0N15sQ="
      ];
      trusted-users = [ "nix-ssh" ];
    };
    extraOptions = ''
      experimental-features = nix-command flakes repl-flake
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
    systemPackages = with pkgs; [ git lsof mc sourceHighlight ];
    shellAliases = {
      ls = "ls --color=auto";
      grep = "grep --color=auto";
      su = "sudo su";
    };
  };

  networking.hosts = {
    "192.168.14.2" = [ "home" ];
    "192.168.14.3" = [ "work" ];
    "192.168.14.4" = [ "parents" ];
    "192.168.14.5" = [ "pc" ];
    "200:3b5c:7981:d112:f1e2:7b3f:218f:e7ea" = [ "home-ygg" ];
    "200:d196:9226:6955:4190:dc33:bac7:17c7" = [ "work-ygg" ];
    "201:613a:44c6:38ad:a0f2:d452:dd0a:94c7" = [ "parents-ygg" ];
    "200:6381:a789:fbbe:3411:2135:e3b:b4a9" = [ "digitalocean-ygg" ];
    "201:b5cd:a295:d34c:a75f:b369:6002:c676" = [ "pc-ygg" ];
  };

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
        extraConfig = { ZSH_AUTOSUGGEST_BUFFER_MAX_SIZE = "10"; };
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
    motd = "Abandon all hope, ye who enter here.";
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
        hashedPasswordFile =
          config.age.secrets.kurnevsky.path or "/secrets/kurnevsky";
        openssh.authorizedKeys.keys = keys;
      };
    };
    groups.secrets = { };
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
    timesyncd.enable = true;
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users.root.programs.ssh.extraOptionOverrides.IdentityAgent =
      "/run/user/1000/gnupg/S.gpg-agent.ssh";
  };
}
