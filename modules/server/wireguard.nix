{
  config,
  ...
}:

{
  networking = {
    firewall = {
      allowedUDPPorts = [
        # WireGuard
        51871
      ];

      trustedInterfaces = [
        "wg0"
      ];
    };

    nat.internalInterfaces = [
      "wg0"
    ];

    wireguard.interfaces.wg0 = {
      ips = [ "192.168.14.1/32" ];
      listenPort = 51871;
      privateKeyFile = config.age.secrets.wg-private.path or "/secrets/wg/private.key";
      peers = [
        {
          publicKey = "aRD0dqodCPyqTklk0KinKiTXYTnIBXZ0WFKy/q0dhQo=";
          presharedKeyFile = config.age.secrets.wg-preshared-dell.path or "/secrets/wg/home.psk";
          allowedIPs = [ "192.168.14.2/32" ];
        }
        {
          publicKey = "v69zSw9Ny+ym3DReKRh0gt+Ecc2rcTyKsieqnVZ/PwE=";
          presharedKeyFile = config.age.secrets.wg-preshared-evo.path or "/secrets/wg/work.psk";
          allowedIPs = [ "192.168.14.3/32" ];
        }
        {
          publicKey = "7Do1rDKMm8dZLgChf8pkS57Cg2A/jEj0JhNEfu0YTHM=";
          presharedKeyFile = config.age.secrets.wg-preshared-acer.path or "/secrets/wg/parents.psk";
          allowedIPs = [ "192.168.14.4/32" ];
        }
        {
          publicKey = "il0KQKwE2+clYFXJT/2mLoC3sRudP3B4g/GR45vlP2E=";
          presharedKeyFile = config.age.secrets.wg-preshared-pc.path or "/secrets/wg/pc.psk";
          allowedIPs = [ "192.168.14.5/32" ];
        }
        {
          publicKey = "B2b19WtF3eKODKHKtA3Nb/z5/5+/VLhKe6IOtxzru1U=";
          allowedIPs = [ "192.168.14.7/32" ];
        }
        {
          publicKey = "+CKl9jJnxex0fbWoLH2z1QSfdjXdkYz/1Juc3ps3WSE=";
          allowedIPs = [ "192.168.14.8/32" ];
        }
      ];
    };
  };
}
