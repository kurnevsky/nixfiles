{
  pkgs,
  ...
}:

{
  # sudo -u forgejo gitea --config /var/lib/forgejo/custom/conf/app.ini
  environment.systemPackages = with pkgs; [ forgejo ];

  services = {
    forgejo = {
      enable = true;
      package = pkgs.forgejo;
      database = {
        type = "postgres";
        createDatabase = true;
      };
      lfs.enable = true;
      settings = {
        server = rec {
          DOMAIN = "git.kropki.org";
          ROOT_URL = "https://${DOMAIN}/";
          HTTP_ADDR = "127.0.0.1";
          HTTP_PORT = 3003;
        };
        service.DISABLE_REGISTRATION = true;
        openid = {
          ENABLE_OPENID_SIGNIN = true;
          ENABLE_OPENID_SIGNUP = true;
          WHITELISTED_URIS = "https://id.kropki.org";
        };
        oauth2_client = {
          OPENID_CONNECT_SCOPES = "profile email groups";
          ENABLE_AUTO_REGISTRATION = true;
          UPDATE_AVATAR = true;
        };
      };
    };

    nginx.virtualHosts."git.kropki.org" = {
      http3 = true;
      quic = true;
      enableACME = true;
      forceSSL = true;
      kTLS = true;
      extraConfig = ''
        client_max_body_size 512M;
      '';
      locations."/".proxyPass = "http://localhost:3003";
    };
  };
}
