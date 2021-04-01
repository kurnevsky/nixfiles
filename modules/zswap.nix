{ ... }:

{
  boot.kernelParams = [ "zswap.enabled=1" ];
  boot.kernelModules = [ "lz4" "z3fold" ];
  systemd.services.zswap-configure = {
    description = "Configure zswap";
    wantedBy = [ "multi-user.target" ];
    serviceConfig.Type = "oneshot";
    script = ''
      echo lz4 > /sys/module/zswap/parameters/compressor
      echo z3fold > /sys/module/zswap/parameters/zpool
    '';
  };
}
