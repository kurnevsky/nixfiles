{
  config,
  ...
}:

{
  networking.firewall.allowedTCPPorts = [
    # SMTP
    25
    # SMTPS
    465
    # SMTPS
    587
    # IMAPS
    993
  ];

  security.acme.certs."stalwart.kropki.org".group = "acme";

  services = {
    postgresql = {
      ensureDatabases = [
        "stalwart-mail"
      ];
      ensureUsers = [
        {
          name = "stalwart-mail";
          ensureDBOwnership = true;
        }
      ];
    };

    stalwart-mail = {
      enable = true;
      settings = {
        storage.blob = "db";
        store = {
          db = {
            type = "postgresql";
            host = "/var/run/postgresql";
            database = "stalwart-mail";
          };
        };
        lookup.default.hostname = "kropki.org";
        server = {
          hostname = "mail.kropki.org";
          listener = {
            smtp = {
              bind = "[::]:25";
              protocol = "smtp";
            };
            submission = {
              bind = [ "[::]:587" ];
              protocol = "smtp";
            };
            submissions = {
              bind = [ "[::]:465" ];
              protocol = "smtp";
              tls.implicit = true;
            };
            imaptls = {
              bind = [ "[::]:993" ];
              protocol = "imap";
              tls.implicit = true;
            };
            jmap = {
              bind = [ "[::]:30452" ];
              protocol = "http";
            };
          };
        };
        authentication.fallback-admin = {
          user = "admin";
          secret = "%{env:ADMIN_SECRET}%";
        };
        auth.dkim.sign = [
          {
            "if" = "listener != 'smtp'";
            "then" = "['ed25519']";
          }
          { "else" = false; }
        ];
        signature.ed25519 = {
          private-key = "%{env:DKIM_KEY}%";
          domain = "kropki.org";
          selector = "default";
          headers = [
            "From"
            "Subject"
            "Date"
            "Message-ID"
            "To"
            "Cc"
            "MIME-Version"
            "Content-Type"
            "Content-Transfer-Encoding"
            "Content-ID"
            "Content-Description"
            "Resent-Date"
            "Resent-From"
            "Resent-Sender"
            "Resent-To"
            "Resent-Cc"
            "Resent-Message-ID"
            "In-Reply-To"
            "References"
            "List-Id"
            "List-Help"
            "List-Unsubscribe"
            "List-Subscribe"
            "List-Post"
            "List-Owner"
            "List-Archive"
          ];
          algorithm = "ed25519-sha256";
          canonicalization = "relaxed/relaxed";
        };
        certificate.default = {
          cert = "%{file:${config.security.acme.certs."kropki.org".directory}/fullchain.pem}%";
          private-key = "%{file:${config.security.acme.certs."kropki.org".directory}/key.pem}%";
          default = true;
        };
        email.encryption.append = true;
        session.rcpt.sub-addressing = [
          {
            "if" = "matches('^([^.]+)\.([^.]+)@(.+)$', rcpt)";
            "then" = "$1 + '@' + $3";
          }
          { "else" = false; }
        ];
        metrics.prometheus.enable = true;
      };
    };

    nginx.virtualHosts."stalwart.kropki.org" = {
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
      locations = {
        "/".proxyPass = "http://localhost:30452";
        "= /metrics/prometheus".return = 403;
      };
    };
  };

  systemd.services.stalwart-mail.serviceConfig = {
    RestrictAddressFamilies = [ "AF_UNIX" ];
    EnvironmentFile = "${config.age.secrets.stalwart.path or "/secrets/stalwart"}";
  };

  users.groups.acme.members = [
    "stalwart-mail"
  ];

  age.secrets.stalwart.file = ../../secrets/stalwart.age;
}
