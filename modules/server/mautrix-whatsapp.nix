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
        encryption = {
          allow = true;
          default = true;
          require = true;
          pickle_key = "$ENCRYPTION_PICKLE_KEY";
          delete_keys = {
            dont_store_outbound = true;
            ratchet_on_decrypt = true;
            delete_fully_used_on_decrypt = true;
            delete_prev_on_new_session = true;
            delete_on_device_delete = true;
            periodically_delete_expired = true;
            delete_outdated_inbound = true;
          };
          verification_levels = {
            receive = "cross-signed-tofu";
            send = "cross-signed-tofu";
            share = "cross-signed-tofu";
          };
        };
      };
    };
  };

  age.secrets.mautrix-whatsapp.file = ../../secrets/mautrix-whatsapp.age;
}
