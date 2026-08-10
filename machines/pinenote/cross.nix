{
  inputs,
  lib,
  ...
}:

let
  pkgsCross = import inputs.nixpkgs {
    system = "x86_64-linux";
    crossSystem.config = "aarch64-unknown-linux-gnu";
  };
in
{
  # Cross-compile the kernel from x86_64 instead of building it in qemu.
  boot.kernelPackages = lib.mkForce (pkgsCross.callPackage ./packages/kernel.nix { });
}
