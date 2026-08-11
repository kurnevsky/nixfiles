{
  config,
  inputs,
  lib,
  ...
}:

let
  pkgsCross = import inputs.nixpkgs {
    system = "x86_64-linux";
    crossSystem = {
      config = "aarch64-unknown-linux-gnu";
      # The platform description defined in pinephone.nix.
      inherit (config.nixpkgs.hostPlatform) linux-kernel;
    };
    overlays = [
      (import "${inputs.mobile-nixos}/overlay/overlay.nix")
      # Same as the overlay defined in mobile-nixos modules/kernel-config.nix.
      (_final: _super: {
        systemBuild-structuredConfig =
          version:
          let
            helpers = lib.kernel // (lib.kernel.whenHelpers version);
          in
          lib.mkMerge (map (fn: fn helpers) config.mobile.kernel.structuredConfig);
      })
    ];
  };
in
{
  # Cross-compile the kernel from x86_64 instead of building it in qemu.
  mobile.boot.stage-1.kernel.package = lib.mkForce (
    pkgsCross.callPackage "${inputs.mobile-nixos}/devices/${config.mobile.device.name}/kernel" { }
  );
}
