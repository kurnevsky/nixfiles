{ pkgs, ... }:

{
  systemd.services.websocat-wg-server = {
    description = "Websocat to wireguard service";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Restart = "on-failure";
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ExecStart = "${pkgs.websocat}/bin/websocat --exit-on-eof --binary ws-listen:127.0.0.1:57411 udp:127.0.0.1:51871";
    };
  };
}
