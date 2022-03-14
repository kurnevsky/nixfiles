{ pkgs, ... }:

let
  pkg = (pkgs.callPackage ./shapeshifter-dispatcher.nix { });
  homeDir = "/var/lib/shapeshifter";
  port = 37815;
in {
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

  systemd.services.shapeshifter-client = {
    description = "Shapeshifter client service";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      ExecStart =
        "${pkg}/bin/shapeshifter-dispatcher -client -transparent -transports obfs4 -state ${homeDir} -target kurnevsky.net:${
          builtins.toString port
        } -proxylistenaddr 127.0.0.1:8022 -optionsFile ${homeDir}/obfs4.json -logLevel INFO -enableLogging";
      User = "shapeshifter";
      PrivateTmp = true;
    };
  };
}
