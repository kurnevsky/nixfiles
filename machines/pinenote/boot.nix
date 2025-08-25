{
  lib,
  pkgs,
  ...
}:

{
  boot = {
    kernelPackages = pkgs.linuxPackages-pinenote;

    kernelParams = [
      "rw"
      "rootwait"
      "earlycon"
      "console=ttyS2,1500000n8"
      "fw_devlink=off"
    ];

    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };

    initrd = {
      includeDefaultModules = false;
      availableKernelModules = lib.mkForce [
        "gpio-rockchip"
        "ext4"
        "mmc_block"
        "usbhid"
        "hid_generic"
      ];
    };

    extraModprobeConfig = ''
      options rockchip_ebc dithering_method=2 default_hint=0xa0 early_cancellation_addition=2 redraw_delay=200
      options brcmfmac feature_disable=0x820001
    '';

    plymouth.enable = true;
  };
}
