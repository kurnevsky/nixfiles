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
