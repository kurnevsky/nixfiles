{
  config,
  lib,
  ...
}:

{
  services.syncthing = {
    enable = true;
    systemService = false;
    openDefaultPorts = true;
  };

  home-manager.users.kurnevsky.services.syncthing = {
    enable = true;
    key = config.age.secrets.syncthing-key.path or "/secrets/syncthing/key";
    cert = config.age.secrets.syncthing-cert.path or "/secrets/syncthing/cert";
    settings = rec {
      devices = lib.filterAttrs (name: _: name != config.networking.hostName) {
        evo.id = "56323HF-7SEVXR2-34IUVUU-PYQYRZ6-OCBF66D-BKWC5V2-W236TTT-57YEIAR";
        pc.id = "ZW66CUX-VYZDALZ-QD4EVPD-2PZ6NMV-G6R7C7J-SRMLFMU-ZOMMXJU-HCZV4AZ";
        android.id = "PPGMSUR-ZZLBZ5W-DC6KZAB-J5EZZIC-QG6TDNP-K24E3X2-IVIZWYX-SIUT2QG";
        digitalocean = {
          addresses = [ "quic://kropki.org" ];
          id = "IIJ6Y4L-VIWHB67-VR74H75-WNFP4G3-5CUY6GD-O6SCUUP-2HWQODL-BBS3CA3";
        };
      };
      folders."/home/kurnevsky/Sync" = {
        id = "Sync";
        devices = lib.attrNames devices;
      };
      options = {
        localAnnounceEnabled = true;
        relaysEnabled = true;
      };
    };
  };
}
