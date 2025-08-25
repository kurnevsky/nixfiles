{ pkgs, fetchFromSourcehut, ... }:

pkgs.linuxPackagesFor (
  (pkgs.linuxKernel.kernels.linux_6_15.override {
    argsOverride = rec {
      src = fetchFromSourcehut {
        owner = "~hrdl";
        repo = "linux";
        rev = "f75fe16d81ae784b8cd2b915113f3a99ff812777";
        sha256 = "sha256-DhMiZMcwownJJRqIYOj87E/j34jJZb2/rTOhYuMumG4=";
      };
      version = "6.15.0-rc3";
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
  }).overrideAttrs
    (old: {
      postInstall = ''
        cp "$out/dtbs/rockchip/rk3566-pinenote-v1.2.dtb" "$out/dtbs/rockchip/pn.dtb"
        ${old.postInstall}
      '';
    })
)
