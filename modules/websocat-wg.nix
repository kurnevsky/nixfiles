{ pkgs, ... }:

{
  systemd.services.websocat-wg = {
    description = "Websocat to wireguard service";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Restart = "on-failure";
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ExecStart = "${pkgs.websocat}/bin/websocat --exit-on-eof --binary udp-listen:127.0.0.1:42930 wss://kropki.org/wswg";
    };
  };
}
