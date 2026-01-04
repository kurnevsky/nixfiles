{
  config,
  ...
}:

{
  services = {
    anki-sync-server = {
      enable = true;
      users = [
        {
          username = "kurnevsky";
          passwordFile = config.age.secrets.anki.path or "/secrets/anki";
        }
      ];
    };

    nginx.virtualHosts."anki.kropki.org" = {
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
      locations."/".proxyPass =
        "http://localhost:${builtins.toString config.services.anki-sync-server.port}";
    };
  };

  age.secrets.anki.file = ../../secrets/anki.age;

  # TODO
  nixpkgs.overlays = [
    (_self: super: {
      anki-sync-server = super.anki-sync-server.overrideAttrs (old: {
        checkFlags = old.checkFlags ++ [
          "--skip=sync::http_client::io_monitor::test::http_success"
          "--skip=test::test_run"
        ];
      });
    })
  ];
}
