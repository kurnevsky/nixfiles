{ pkgs, lib, ... }:

{
  nixpkgs.config.allowUnfreePredicate = pkg:
    builtins.elem (lib.getName pkg) [ "symbola" "unrar" "p7zip" ];

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

  environment = {
    systemPackages = with pkgs; [ git mc ];
    shellAliases = {
      ls = "ls --color=auto";
      grep = "grep --color=auto";
      su = "sudo su";
    };
  };

  networking.extraHosts = ''
    192.168.14.2 home
    192.168.14.3 work
    192.168.14.4 parents
    200:3b5c:7981:d112:f1e2:7b3f:218f:e7ea home-ygg
    200:d196:9226:6955:4190:dc33:bac7:17c7 work-ygg
    201:613a:44c6:38ad:a0f2:d452:dd0a:94c7 parents-ygg
    200:6381:a789:fbbe:3411:2135:e3b:b4a9 digitalocean-ygg
  '';

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
    less = {
      enable = true;
      lessopen = "| ${pkgs.sourceHighlight}/bin/src-hilite-lesspipe.sh %s";
      envVariables.LESS = " -R ";
    };
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
      };
      kurnevsky = {
        uid = 1000;
        isNormalUser = true;
        extraGroups = [ "wheel" ];
        shell = pkgs.zsh;
        passwordFile = "/secrets/kurnevsky";
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCnphYFSCOiMmmvpktspcCc+Qkg8GArzaB3aSxuIgUOQzS/uk+shTSbnHsP/ABvtGPWz3Fc0XJzRkDVQpYaSdpdMWXR7RgxeZ0GzB4954mV+e3wD8qN+4zlQY+g/ablv9HEPnjOrkbwXGERnj/4DVBJQhcrBB6GKIWk7YGOek1Bp70CUp9jdaLhnuh6qnvIRb/CDR9aIDZoQjt9O4DcUJYRsQOdS7QgxzRG8loiy6ur82GarLVR1aQAq8dB46eDZDBS91WxrzrWtk1T7UBg7MKNmQdyVw5aqh6V6s+fpOn+KmjajD0DOIspDNmFe3+TkPEqGFJGMTrqCrWSqtxyhPqJ kurnevsky@gmail.com"
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDDPmgbv8HY8cevVCgZPJw+4WQzXxmV8RU0r0owdFAmcMFHc0SLeFtWvZ2fV6LkerPKHzK1uWTwPq1RhqMDVlhzuaDjtgLlNX4VJ84aHzjv62+6gaAbUzEkIwF3nigVI8MECW7r1Sk38yI42VaGn8Qa2ThKwdbqcskeh8eD0TyvVSNe46vz4AnMR/gw2bkkGIIUTWF6MP8/uMbxdErLUSZPaoflfO3RpMQPomNigrgwDxptisY2nWhTSskOu+RVj17yBIQIH0d4EpiezRniQ1YeI47LSj7I01e/zy1HyyEm1S/mKe+uaHDIlcGWllWXam9AKC5atyUiH9lbj0c1vUe9WtP0dk8Zf2qgJwkB0DZAhehVbycw4rP4omUisI/rZjUxXOFk2R/O5asxbtIWsLjAJIW8g6uf9e6T0+5piAuyF3fd3zy4ZIj5/G2EAsywxxB4Jec5kKCHOy4E6tFgF2jtLAgTk4dij/dZVvZsUWYAxBdZjQ7yUIHVCNUU2Br4+NvtyoW7/2JH8EQP+agPCuUVMF0SdWUxhXfDbojEAO9y71D2PiDZwyFAPY15e0hMI80r1A6bZxRiBeufUxnimeGSuBqzUhiBBjGgT3Cm0amJ5ZGSggqss+txVEn2Ntgbi9SZ9X4BiQdE6zylIsFpSVEZ8KYIySxkK2ElQp7XqCaqDw== ykurneuski@evolution.com"
        ];
      };
    };
  };

  services = {
    openssh = {
      enable = true;
      permitRootLogin = "no";
      passwordAuthentication = false;
      # TODO: rename to kbdInteractiveAuthentication after new nixos release
      challengeResponseAuthentication = false;
    };
    timesyncd.enable = true;
  };
}
