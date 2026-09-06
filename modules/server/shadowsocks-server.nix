{ config, pkgs, ... }:

let
  shadowsocksConfig = {
    server = "0.0.0.0";
    server_port = 29135;
    method = "chacha20-ietf-poly1305";
    mode = "tcp_and_udp";
    fast_open = true;
  };
  shadowsocksConfigFile = pkgs.writeText "shadowsocks.json" (builtins.toJSON shadowsocksConfig);
in
{
  networking.firewall = {
    allowedTCPPorts = [
      # Shadowsocks
      29135
    ];
    allowedUDPPorts = [
      # Shadowsocks
      29135
    ];
  };

  systemd.services.shadowsocks-server = {
    description = "Shadowsocks server service";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      RuntimeDirectory = "shadowsocks-server";
      LoadCredential = [
        "password:${config.age.secrets.shadowsocks.path or "/secrets/shadowsocks"}"
      ];
    };
    script = ''
      ${pkgs.jq}/bin/jq --arg password "$(cat "$CREDENTIALS_DIRECTORY/password")" \
        '. + { password: $password }' ${shadowsocksConfigFile} > "$RUNTIME_DIRECTORY/shadowsocks.json"
      exec ${pkgs.shadowsocks-rust}/bin/ssserver --config "$RUNTIME_DIRECTORY/shadowsocks.json"
    '';
  };

  age.secrets.shadowsocks.file = ../../secrets/shadowsocks.age;
}
