{ pkgs, ... }:

let
  shadowsocksConfig = {
    server = "kurnevsky.net";
    server_port = 29135;
    method = "chacha20-ietf-poly1305";
    mode = "tcp_and_udp";
    fast_open = true;

    locals = [
      {
        protocol = "socks";
        local_address = "127.0.0.1";
        local_port = 1080;
      }
      {
        protocol = "tunnel";
        mode = "udp_only";
        local_address = "127.0.0.1";
        local_port = 51870;
        forward_address = "127.0.0.1";
        forward_port = 51871;
      }
    ];
  };
  shadowsocksConfigFile =
    pkgs.writeText "shadowsocks.json" (builtins.toJSON shadowsocksConfig);
in {
  systemd.services.shadowsocks-client = {
    description = "Shadowsocks client service";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      DynamicUser = true;
      PrivateTmp = true;
      ProtectSystem = "strict";
      SupplementaryGroups = "secrets";
    };
    script = ''
      cat ${shadowsocksConfigFile} | ${pkgs.jq}/bin/jq --arg password "$(cat /secrets/shadowsocks)" '. + { password: $password }' > /tmp/shadowsocks.json
      exec ${pkgs.shadowsocks-rust}/bin/sslocal --config /tmp/shadowsocks.json
    '';
  };
}
