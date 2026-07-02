{ pkgs, fetchFromSourcehut, ... }:

pkgs.linuxPackagesFor (
  (
    let
      kernelPatches = pkgs.callPackage "${pkgs.path}/pkgs/os-specific/linux/kernel/patches.nix" { };
      kernel = pkgs.callPackage "${pkgs.path}/pkgs/os-specific/linux/kernel/mainline.nix" {
        branch = "6.19";
        kernelPatches = [
          kernelPatches.bridge_stp_helper
          kernelPatches.request_key_helper
        ];
      };
    in
    kernel.override {
      argsOverride = rec {
        src = fetchFromSourcehut {
          owner = "~hrdl";
          repo = "linux";
          rev = "46028a0e2658877625568f2134e243b304966ef4";
          sha256 = "sha256-Y96Yae3SLoTJoRLqjeMK7lubBCsJUrc/1Lk6Eyfm6K0=";
        };
        version = "6.19.0";
        modDirVersion = version;
        defconfig = "pinenote_defconfig";

        # These are required to make the build work:
        extraConfig = ''
          VIDEO_THP7312 n
          CRYPTO_AEGIS128_SIMD n
          ROCKCHIP_DW_HDMI_QP n
        '';
        ignoreConfigErrors = true;
      };
    }
  ).overrideAttrs
    (old: {
      postInstall = ''
        cp "$out/dtbs/rockchip/rk3566-pinenote-v1.2.dtb" "$out/dtbs/rockchip/pn.dtb"
        ${old.postInstall}
      '';
    })
)
