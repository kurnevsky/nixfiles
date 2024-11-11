{
  # system.replaceRuntimeDependencies can be used to make fast fixes
  nixpkgs.overlays = [
    (_self: super: {
      vaapiIntel = super.vaapiIntel.override { enableHybridCodec = true; };
      deadbeef = super.deadbeef.override { wavpackSupport = true; };
      deadbeef-with-plugins = super.deadbeef-with-plugins.override {
        plugins = with super.deadbeefPlugins; [ mpris2 statusnotifier ];
      };
      pass = super.pass-wayland.withExtensions
        (ext: with ext; [ pass-otp pass-update ]);
      firefox = super.firefox.override {
        nativeMessagingHosts =
          [ super.passff-host super.kdePackages.plasma-browser-integration ];
      };
      mpv = super.mpv.override { scripts = with super.mpvScripts; [ mpris ]; };
      _7zz = super._7zz.override { enableUnfree = true; };
      p7zip = super.p7zip.override { enableUnfree = true; };
      isync = super.symlinkJoin {
        name = "isync";
        paths = [
          (super.writeShellScriptBin "mbsync" ''
            export SASL_PATH=${super.cyrus_sasl.out}/lib/sasl2:${super.cyrus-sasl-xoauth2}/lib/sasl2
            exec ${super.isync}/bin/mbsync "$@"
          '')
          super.isync
        ];
      };
      viu = super.viu.override { withSixel = true; };
      cloud-mdir-sync = super.callPackage ./cloud-mdir-sync.nix { };
    })
    (_self: super: {
      tor-browser-bundle-bin = super.symlinkJoin {
        inherit (super.tor-browser-bundle-bin) name;
        paths = [ super.tor-browser-bundle-bin ];
        buildInputs = [ super.makeWrapper ];
        postBuild = ''
          wrapProgram "$out/bin/tor-browser" \
            --set MOZ_ENABLE_WAYLAND 1
        '';
      };
    })
    (_self: super:
      let
        javaPath = "/tmp/java";
        wrap = drv: name:
          super.symlinkJoin {
            inherit (drv) name;
            paths = [
              (super.writeShellScriptBin name ''
                ${super.bubblewrap}/bin/bwrap \
                  --bind / / \
                  --dev-bind /dev /dev \
                  --proc /proc \
                  --ro-bind ${super.jdk}/lib/openjdk ${javaPath} \
                  --setenv JAVA_HOME ${javaPath} \
                  ${drv}/bin/${name} "$@"
              '')
              drv
            ];
          };
      in {
        metals = wrap super.metals "metals";
        bloop = wrap super.bloop "bloop";
      })
    # TODO:
    # (_self: super: {
    #   telegram-desktop = super.telegram-desktop.overrideAttrs (old: {
    #     patches = let
    #       baseUrl =
    #         "https://raw.githubusercontent.com/kurnevsky/telegram-desktop-patches/03c1041f13ffbbae18d25bef65de6040318fc4a2/";
    #     in (old.patches or [ ]) ++ [
    #       (super.fetchpatch {
    #         url = baseUrl + "0001-Disable-sponsored-messages.patch";
    #         sha256 = "sha256-HeDH6tkkGx2XYTtzfo+gRee4BYxRiPKXQuftycl8Kvo=";
    #       })
    #       (super.fetchpatch {
    #         url = baseUrl + "0002-Disable-saving-restrictions.patch";
    #         sha256 = "sha256-YarWT2rDNoOpLt0jGuT5BAe662GG9TMWF/F7KGa3I0E=";
    #       })
    #       (super.fetchpatch {
    #         url = baseUrl + "0003-Disable-invite-peeking-restrictions.patch";
    #         sha256 = "sha256-8mJD6LOjz11yfAdY4QPK/AUz9o5W3XdupXxy7kRrbC8=";
    #       })
    #       (super.fetchpatch {
    #         url = baseUrl + "0004-Disable-accounts-limit.patch";
    #         sha256 = "sha256-PZWCFdGE/TTJ1auG1JXNpnTUko2rCWla6dYKaQNzreg=";
    #       })
    #       (super.fetchpatch {
    #         url = baseUrl + "0005-Option-to-disable-stories.patch";
    #         sha256 = "sha256-aSAjyFiOg8JLgYA3voijVvkGIgK93kNMx40vqHsvW8Y=";
    #       })
    #     ];
    #   });
    # })
  ];
}
