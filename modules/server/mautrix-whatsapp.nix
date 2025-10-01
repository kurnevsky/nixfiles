{
  pkgs,
  config,
  ...
}:

{
  nixpkgs.config.permittedInsecurePackages = [
    "olm-3.2.16"
  ];

  services = {
    postgresql = {
      ensureDatabases = [
        "mautrix-whatsapp"
      ];
      ensureUsers = [
        {
          name = "mautrix-whatsapp";
          ensureDBOwnership = true;
        }
      ];
    };

    mautrix-whatsapp = {
      enable = true;
      serviceDependencies = ["postgresql.service" "continuwuity.service"];
      environmentFile = config.age.secrets.mautrix-whatsapp.path or "/secrets/mautrix-whatsapp";
      settings = {
        network.disable_status_broadcast_send = false;
        bridge.permissions = {
          "@admin:kropki.org" = "admin";
          "kropki.org" = "user";
        };
        database = {
          type = "postgres";
          uri = "postgres:///mautrix-whatsapp?host=/var/run/postgresql";
        };
        homeserver = {
          address = "https://kropki.org:8448";
          domain = "kropki.org";
        };
        matrix.delivery_receipts = true;
        backfill.enabled = true;
      };
    };
  };

  age.secrets.mautrix-whatsapp.file = ../../secrets/mautrix-whatsapp.age;
}
