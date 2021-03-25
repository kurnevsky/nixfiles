{
  inputs = {
    nixpkgs = {
      type = "github";
      owner = "NixOS";
      repo = "nixpkgs";
      ref = "nixos-20.09";
    };

    emacs-overlay = {
      type = "github";
      owner = "nix-community";
      repo = "emacs-overlay";
    };
  };

  outputs = { self, ... }@inputs: {
    nixosConfigurations.nixos = inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [
        (args: { nixpkgs.overlays = [ inputs.emacs-overlay.overlay ]; })
        ./shadowsocks.nix
        ./configuration.nix
      ];
    };
  };
}
