{
  pkgs,
  lib,
  ...
}:

{
  services.udev.packages = [
    (pkgs.writeTextDir "lib/udev/rules.d/81-libinput-pinenote.rules" ''
      ACTION=="remove", GOTO="libinput_device_group_end"
      KERNEL!="event[0-9]*", GOTO="libinput_device_group_end"

      ATTRS{phys}=="?*", ATTRS{name}=="cyttsp5", ENV{LIBINPUT_DEVICE_GROUP}="pinenotetouch"
      ATTRS{phys}=="?*", ATTRS{name}=="w9013 2D1F:0095 Stylus", ENV{LIBINPUT_DEVICE_GROUP}="pinenotetouch"
      #ATTRS{phys}=="?*", ATTRS{name}=="w9013 2D1F:0095 Stylus", ENV{ID_INPUT_HEIGHT_MM}=""
      #ATTRS{phys}=="?*", ATTRS{name}=="w9013 2D1F:0095 Stylus", ENV{ID_INPUT_WIDTH_MM}=""
      #ATTRS{phys}=="?*", ATTRS{name}=="cyttsp5", PROGRAM=="/usr/local/bin/is_smaeul_kernel", ENV{LIBINPUT_CALIBRATION_MATRIX}="-1 0 1 0 -1 1"
      ATTRS{phys}=="?*", ATTRS{name}=="cyttsp5", ENV{LIBINPUT_CALIBRATION_MATRIX}="-1 0 1 0 -1 1"

      LABEL="libinput_device_group_end"
    '')

    (pkgs.writeTextDir "lib/udev/rules.d/83-backlight.rules" ''
      SUBSYSTEM=="backlight", ACTION=="add", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
    '')

    (pkgs.writeTextDir "lib/udev/rules.d/84-rockchip-ebc-power.rules" ''
      DRIVER=="rockchip-ebc", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/%p/power/control", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/%p/power/control"
    '')
  ];

  # libinput quirks for touch input
  environment.etc."libinput/local-overrides.quirks".text = ''
    [PineNote]
    MatchName=cyttsp5
    AttrPalmPressureThreshold=27
    AttrThumbPressureThreshold=28
    AttrSizeHint=210x157
    #AttrResolutionHint=4x4
    #AttrPalmSizeThreshold=1
  '';

  hardware = {
    # workaround: current uboot has a 127 char limit for the path
    deviceTree.name = "rockchip/pn.dtb";

    firmware = with pkgs; [
      pinenote-firmware
      raspberrypiWirelessFirmware
    ];

    graphics.enable = true;
  };

  systemd.services.pinenote-init-convert-waveform = {
    description = "Convert rockchip_ebc ebc.wbf waveform to custom_wf.bin";
    wantedBy = [ "multi-user.target" ];
    serviceConfig =
      let
        firmwareDir = "/lib/firmware/rockchip";
      in
      {
        Type = "oneshot";
        ExecStart = "${pkgs.writeShellScript "init-waveform.sh" ''
          if [ -e ${firmwareDir}/custom_wf.bin ]; then
            echo "Nothing to do (${firmwareDir}/custom_wf.bin already exists)"
            exit 0
          fi

          if [ -e ${firmwareDir}/ebc.wbf ]; then
            echo "Removing old extraction file ${firmwareDir}/ebc.wbf"
            rm -f "${firmwareDir}/ebc.wbf"
          fi

          echo "Extracting waveforms"
          mkdir -vp ${firmwareDir}
          ${pkgs.hrdl-utils}/bin/waveform_extract.sh

          echo "Converting waveforms"
          cd /tmp
          ${pkgs.hrdl-utils}/bin/wbf_to_custom.py ${firmwareDir}/ebc.wbf
          mv -v custom_wf.bin ${firmwareDir}/

          echo "Reloading driver"
          modprobe -v -r rockchip_ebc
          modprobe -v rockchip_ebc

          echo "Done"
        ''}";
      };
  };

  nixpkgs.overlays = [
    (_self: super: {
      pinenote-firmware = pkgs.callPackage ./packages/pinenote-firmware.nix { };
      hrdl-utils = super.callPackage ./packages/hrdl-utils.nix { };
      linuxPackages-pinenote = super.callPackage ./packages/kernel.nix { };
    })
  ];
}
