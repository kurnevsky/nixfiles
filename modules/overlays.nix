{ pkgs, nixpkgs-unstable, ... }:

{
  # system.replaceRuntimeDependencies can be used to make fast fixes
  nixpkgs.overlays = [
    (self: super: {
      vaapiIntel = super.vaapiIntel.override { enableHybridCodec = true; };
      deadbeef = super.deadbeef.override { wavpackSupport = true; };
      deadbeef-with-plugins = super.deadbeef-with-plugins.override {
        plugins = with super.deadbeefPlugins; [
          mpris2
          # TODO: upstream
          (super.callPackage (builtins.fetchurl {
            url =
              "https://raw.githubusercontent.com/kurnevsky/nixpkgs/deadbeef-statusnotifier-plugin-libdbusmenu/pkgs/applications/audio/deadbeef/plugins/statusnotifier.nix";
            sha256 =
              "sha256:18780hbqhpv9v1kyhipz2kzd3avqjd9lv98ji7ip9qvafmr2c62k";
          }) { })
        ];
      };
      zip-natspec = super.zip.override { enableNLS = true; };
      unzip-natspec = super.unzip.override { enableNLS = true; };
      firefox = super.firefox.override {
        extraNativeMessagingHosts = [ super.passff-host ];
      };
      firefox-wayland = super.firefox-wayland.override {
        extraNativeMessagingHosts = [ super.passff-host ];
      };
      mpv-with-scripts = super.mpv-with-scripts.override {
        scripts = with pkgs.mpvScripts; [ mpris ];
      };
      p7zip = super.p7zip.override { enableUnfree = true; };
      pidgin-with-plugins = super.pidgin.override {
        plugins = with pkgs; [
          pidgin-otr
          pidgin-xmpp-receipts
          pidgin-skypeweb
          pidgin-carbons
          purple-lurch
          purple-plugin-pack
          purple-slack
          # TODO: hash mismatch
          # telegram-purple
          (pkgs.callPackage ./pidgin-indicator.nix { })
        ];
      };
      ungoogled-chromium-wayland = super.ungoogled-chromium.override {
        commandLineArgs = "--ozone-platform-hint=auto";
      };
      tor-browser-bundle-bin =
        nixpkgs-unstable.tor-browser-bundle-bin.override {
          useHardenedMalloc = false;
        };
    })
    # will be upstreamed eventually
    (self: super: {
      onnxruntime = pkgs.callPackage (builtins.fetchurl {
        url =
          "https://raw.githubusercontent.com/NixOS/nixpkgs/708625f80a0ce815e8ee5f396621e89dd8edc53d/pkgs/development/libraries/onnxruntime/default.nix";
        sha256 = "sha256:0w24fh2l4y5p9pg3ykjpnvz70k6qrrgrlah861nljjrc7n6g8j6l";
      }) { };
      obs-backgroundremoval = (pkgs.callPackage (builtins.fetchurl {
        url =
          "https://raw.githubusercontent.com/puffnfresh/nixpkgs/6630869e12cfeba50e1e370e26255ed8c3f46832/pkgs/applications/video/obs-studio/plugins/obs-backgroundremoval.nix";
        sha256 = "sha256:1cxld16p41vm5yvfrjnmsn2w9rvwfgl9jmjql302wi4wrvkyh2cc";
      }) { }).overrideAttrs (old: {
        patches = [
          (builtins.fetchurl {
            url =
              "https://raw.githubusercontent.com/puffnfresh/nixpkgs/6630869e12cfeba50e1e370e26255ed8c3f46832/pkgs/applications/video/obs-studio/plugins/obs-backgroundremoval-includes.patch";
            sha256 =
              "sha256:1w31nbcdd3n801g1660dsbndmnbq2h1w4knbl5wdzbn7zqpyb8n1";
          })
        ];
      });
    })
  ];
}
