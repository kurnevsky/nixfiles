{ pkgs, ... }:

{
  systemd.services.websocat-ssh = {
    description = "Websocat to ssh service";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      Restart = "on-failure";
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      ExecStart = "${pkgs.websocat}/bin/websocat --exit-on-eof --binary tcp-listen:127.0.0.1:26203 wss://kropki.org/wssh";
    };
  };
}
