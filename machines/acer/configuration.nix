{ ... }:

{
  boot = {
    cleanTmpDir = true;
    kernelPatches = [{
      name = "nouveau";
      patch = ./nouveau.patch;
    }];
  };

  system.stateVersion = "21.11";

  home-manager.users = {
    root.home.stateVersion = "21.11";
    parents.home.stateVersion = "21.11";
    kurnevsky.home.stateVersion = "21.11";
  };
}
