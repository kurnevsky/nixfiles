{ pkgs, ... }:

let
  shadowsocksConfig = {
    server = "kurnevsky.net";
    server_port = 443;
    local_address = "127.0.0.1";
    local_port = 1080;
    timeout = 300;
    method = "chacha20-ietf-poly1305";
    fast_open = true;
    workers = 1;
    plugin = "v2ray-plugin";
    plugin_opts = "tls;host=kurnevsky.net;path=/ss";
  };
  shadowsocksConfigFile =
    pkgs.writeText "shadowsocks.json" (builtins.toJSON shadowsocksConfig);
in {
  systemd.services.shadowsocks-client = {
    description = "Shadowsocks client service";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    path = [ pkgs.shadowsocks-rust pkgs.jq pkgs.shadowsocks-v2ray-plugin ];
    serviceConfig = {
      User = "nobody";
      PrivateTmp = true;
    };
    script = ''
      cat ${shadowsocksConfigFile} | jq --arg password "$(cat /secrets/shadowsocks)" '. + { password: $password }' > /tmp/shadowsocks.json
      exec sslocal --config /tmp/shadowsocks.json
    '';
  };
}
