{ pkgs, ... }:

{
  age.ageBin = "${pkgs.rage}/bin/rage";

  nixpkgs.overlays = [
    (_self: super: {
      agenix = super.agenix.override { ageBin = "${pkgs.rage}/bin/rage"; };
    })
  ];
}
