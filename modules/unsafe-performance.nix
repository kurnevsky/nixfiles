{
  boot = {
    kernel.sysctl."kernel.split_lock_mitigate" = 0;
    kernelParams = [ "mitigations=off" ];
  };
}
