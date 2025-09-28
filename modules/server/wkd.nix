{
  services.nginx.virtualHosts."kropki.org".locations = {
    "= /.well-known/openpgpkey/policy".return = "200";
    "/.well-known/openpgpkey/hu/" = {
      alias = "/srv/www/wkd/";
      tryFiles = "$uri =404";
      extraConfig = ''
        expires 24h;
        add_header Content-Type application/octet-stream;
        add_header Access-Control-Allow-Methods GET;
        add_header Access-Control-Allow-Origin *;
        add_header Access-Control-Max-Age 86400;
      '';
    };
  };
}
