{
  disabledModules = [ "services/networking/i2pd.nix" ];
  imports = [
    (builtins.fetchurl {
      url = "https://raw.githubusercontent.com/NixOS/nixpkgs/97eec826d5c0098dd34cc6c2c8b87d2aa702a656/nixos/modules/services/networking/i2pd.nix";
      sha256 = "sha256:0c5j446yljjkyj7qwb4f41k9yy77imaxmmp2rgvx4pb8qlzx24i0";
    })
  ];

  networking.firewall = {
    allowedUDPPorts = [
      16964
    ];
    allowedTCPPorts = [
      16964
    ];
  };

  services.i2pd = {
    enable = true;
    enableIPv6 = true;
    nat = false;
    floodfill = true;
    bandwidth = 2048;
    share = 90;
    port = 16964;
    inTunnels.ssh = {
      address = "127.0.0.1";
      port = 22;
      keys = "kropki.dat";
      outbound.length = 1;
      inbound.length = 1;
    };
    yggdrasil.enable = true;
  };
}
