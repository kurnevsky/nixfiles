{ pkgs, nixpkgs-master, ... }:

{
  nixpkgs.overlays = [
    (self: super: {
      deadbeef = super.deadbeef.override { wavpackSupport = true; };
      deadbeef-with-plugins = super.deadbeef-with-plugins.override {
        plugins = [
          super.deadbeef-mpris2-plugin
          # TODO: use plugin from nixpkgs after nixos release
          (pkgs.callPackage ./deadbeef-statusnotifier-plugin.nix { })
        ];
      };
      zip-natspec = super.zip.override { enableNLS = true; };
      unzip-natspec = super.unzip.override { enableNLS = true; };
      firefox = super.firefox.override {
        extraNativeMessagingHosts = [ super.passff-host ];
      };
      mpv-with-scripts = super.mpv-with-scripts.override {
        scripts = with pkgs.mpvScripts; [ mpris ];
      };
      p7zip = super.p7zip.override { enableUnfree = true; };
      pidgin-with-plugins = super.pidgin-with-plugins.override {
        plugins = with pkgs; [
          pidgin-otr
          pidgin-xmpp-receipts
          pidgin-skypeweb
          pidgin-carbons
          purple-lurch
          purple-plugin-pack
          purple-slack
          (pkgs.callPackage ./pidgin-indicator.nix { })
        ];
      };
      tor-browser-bundle-bin = nixpkgs-master.tor-browser-bundle-bin;
    })
  ];
}
