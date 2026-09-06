{
  boot.kernelParams = [ "brcmfmac.feature_disable=0x200000" ];

  networking.hostName = "pinephone-pro";

  age.secrets = {
    kurnevsky.file = ../../secrets/kurnevsky-pinephone-pro.age;
    github.file = ../../secrets/github.age;
    store.file = ../../secrets/store-pinephone-pro.age;
    hans.file = ../../secrets/hans.age;
    iodine.file = ../../secrets/iodine.age;
    shadowsocks.file = ../../secrets/shadowsocks.age;
  };

  system.stateVersion = "25.11";

  home-manager.users = {
    root.home.stateVersion = "25.11";
    kurnevsky.home.stateVersion = "25.11";
  };
}
