{ pkgs, ... }:

let
  shadowsocksConfig = {
    server = "127.0.0.1";
    server_port = 8388;
    timeout = 300;
    method = "chacha20-ietf-poly1305";
    fast_open = true;
    workers = 1;
    plugin = "v2ray-plugin";
    plugin_opts = "server;path=/ss";
  };
  shadowsocksConfigFile =
    pkgs.writeText "shadowsocks.json" (builtins.toJSON shadowsocksConfig);
in {
  systemd.services.shadowsocks-server = {
    description = "Shadowsocks server service";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.shadowsocks-rust pkgs.jq pkgs.shadowsocks-v2ray-plugin ];
    serviceConfig = {
      DynamicUser = true;
      PrivateTmp = true;
      SupplementaryGroups = "secrets";
    };
    script = ''
      cat ${shadowsocksConfigFile} | jq --arg password "$(cat /secrets/shadowsocks)" '. + { password: $password }' > /tmp/shadowsocks.json
      exec ssserver --config /tmp/shadowsocks.json
    '';
  };
}
