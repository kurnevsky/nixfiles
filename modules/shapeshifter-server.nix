{ pkgs, ... }:

let
  pkg = (pkgs.callPackage ./shapeshifter-dispatcher.nix { });
  homeDir = "/var/lib/shapeshifter";
  port = 37815;
in {
  networking.firewall.allowedTCPPorts = [ port ];

  users = {
    users.shapeshifter = {
      group = "shapeshifter";
      description = "Shapeshifter Service user";
      home = homeDir;
      createHome = true;
      isSystemUser = true;
    };
    groups.shapeshifter = { };
  };

  systemd.services.shapeshifter-server = {
    description = "Shapeshifter server service";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      ExecStart =
        "${pkg}/bin/shapeshifter-dispatcher -server -transparent -transports obfs4 -state ${homeDir} -target 127.0.0.1:22 -bindaddr obfs4-0.0.0.0:${
          builtins.toString port
        } -logLevel INFO -enableLogging";
      User = "shapeshifter";
      PrivateTmp = true;
    };
  };
}
