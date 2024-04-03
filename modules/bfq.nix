{
  boot.kernelModules = [ "bfq" ];
  services.udev.extraRules = ''
    ACTION=="add|change", KERNEL=="[sv]d*[!0-9]|sr[0-9]*|loop[0-9]*|mmcblk[0-9]*|nvme[0-9]*", SUBSYSTEM=="block", ATTR{queue/scheduler}="bfq"
  '';
}
