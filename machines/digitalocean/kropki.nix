{
  pkgs,
  config,
  ...
}:

{
  services.postgresql = {
    ensureDatabases = [
      "kropki"
    ];
    ensureUsers = [
      {
        name = "kropki";
        ensureDBOwnership = true;
      }
    ];
  };

  systemd.services.kropki = {
    description = "Kropki server";
    after = [ "network.target" ];
    wants = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Restart = "on-failure";
      User = "kropki";
      Group = "kropki";
      PrivateTmp = true;
      ProtectSystem = "strict";
      Environment = [ "POSTGRES_SOCKET=/var/run/postgresql" ];
      EnvironmentFile = "${config.age.secrets.kropki.path or "/secrets/kropki"}";
      ExecStart = "${pkgs.callPackage ./kropki-server.nix { }}/bin/kropki";
    };
  };
  tt-rss.wantedBy = pkgs.lib.mkForce [ ];

  users = {
    users.kropki = {
      group = "kropki";
      isSystemUser = true;
    };
    groups.kropki = { };
  };

  age.secrets.kropki.file = ../../secrets/kropki.age;
}
