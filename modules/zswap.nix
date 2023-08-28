{ ... }:

{
  boot = {
    kernelParams = [ "zswap.enabled=1" ];
    initrd = {
      kernelModules = [ "lz4" "z3fold" ];
      preDeviceCommands = ''
        echo lz4 > /sys/module/zswap/parameters/compressor
        echo z3fold > /sys/module/zswap/parameters/zpool
      '';
    };
  };
}
