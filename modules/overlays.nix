{
  # system.replaceRuntimeDependencies can be used to make fast fixes
  nixpkgs.overlays = [
    (_self: super: {
      vaapiIntel = super.vaapiIntel.override { enableHybridCodec = true; };
      deadbeef = super.deadbeef.override { wavpackSupport = true; };
      deadbeef-with-plugins = super.deadbeef-with-plugins.override {
        plugins = with super.deadbeefPlugins; [
          mpris2
          statusnotifier
        ];
      };
      pass = super.pass-wayland.withExtensions (
        ext: with ext; [
          pass-otp
          pass-update
        ]
      );
      firefox = super.firefox.override {
        nativeMessagingHosts = [
          super.passff-host
          super.kdePackages.plasma-browser-integration
        ];
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
    (
      _self: super:
      let
        javaPath = "/tmp/java";
        wrap =
          drv: name:
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
      in
      {
        metals = wrap super.metals "metals";
        bloop = wrap super.bloop "bloop";
      }
    )
    (_self: super: { wine-ge = super.callPackage ./wine-ge.nix { }; })
    (_self: super: {
      telegram-desktop = super.telegram-desktop.override {
        unwrapped = super.telegram-desktop.unwrapped.overrideAttrs (old: {
          patches = (old.patches or [ ]) ++ [
            ./telegram/0001-Disable-sponsored-messages.patch
            ./telegram/0002-Disable-saving-restrictions.patch
            ./telegram/0003-Disable-invite-peeking-restrictions.patch
            ./telegram/0004-Disable-accounts-limit.patch
          ];
        });
      };
    })
  ];
}
