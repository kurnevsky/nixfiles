{
  networking.firewall = {
    allowedTCPPorts = [
      # Yggdrasil
      42853
    ];
    allowedUDPPorts = [
      # Yggdrasil
      42853
    ];
  };

  services.yggdrasil = {
    enable = true;
    settings = {
      Peers = [
        "tls://45.147.198.155:6010"
        "tls://ygg.mkg20001.io:443"
        "quic://vpn.itrus.su:7993"
      ];
      Listen = [
        "tls://0.0.0.0:42853"
        "quic://0.0.0.0:42853"
      ];
    };
    persistentKeys = true;
  };
}
