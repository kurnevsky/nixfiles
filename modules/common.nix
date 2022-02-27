{ pkgs, ... }:

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

  environment.systemPackages = [ pkgs.git ];

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

  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
    # TODO: rename to kbdInteractiveAuthentication after new nixos release
    challengeResponseAuthentication = false;
  };
}
