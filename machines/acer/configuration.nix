{ pkgs, ... }:

{
  boot = {
    cleanTmpDir = true;
    kernelPatches = [{
      name = "nouveau";
      patch = ./nouveau.patch;
    }];
  };

  systemd.services.eurodollar = {
    description = "Setkeycodes for € and $ keys";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = [
        "${pkgs.kbd}/bin/setkeycodes b3 183"
        "${pkgs.kbd}/bin/setkeycodes b4 184"
      ];
    };
  };

  system.stateVersion = "21.11";

  home-manager.users = {
    root.home.stateVersion = "21.11";
    parents.home.stateVersion = "21.11";
    kurnevsky.home.stateVersion = "21.11";
  };
}
