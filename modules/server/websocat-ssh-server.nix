{ pkgs, ... }:

{
  services.nginx.virtualHosts."kropki.org".locations."/wssh" = {
    proxyPass = "http://localhost:58546";
    proxyWebsockets = true;
  };

  systemd.services.websocat-ssh-server = {
    description = "Websocat to ssh service";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Restart = "on-failure";
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ExecStart = "${pkgs.websocat}/bin/websocat --exit-on-eof --binary ws-listen:127.0.0.1:58546 tcp:127.0.0.1:22";
    };
  };
}
