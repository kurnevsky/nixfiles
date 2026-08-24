{
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
    settings = {
      ipv6 = true;
      meshnets.yggdrasil = true;
      floodfill = true;
      bandwidth = 2048;
      share = 90;
      port = 16964;
    };
    serverTunnels.ssh = {
      host = "127.0.0.1";
      port = 22;
      keys = "kropki.dat";
      "outbound.length" = 1;
      "inbound.length" = 1;
    };
  };
}
