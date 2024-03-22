{
  # system.replaceRuntimeDependencies can be used to make fast fixes
  nixpkgs.overlays = [
    (self: super: {
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
      cloud-mdir-sync = (super.callPackage ./cloud-mdir-sync.nix { });
    })
    (self: super: {
      tor-browser-bundle-bin = super.symlinkJoin {
        name = super.tor-browser-bundle-bin.name;
        paths = [ super.tor-browser-bundle-bin ];
        buildInputs = [ super.makeWrapper ];
        postBuild = ''
          wrapProgram "$out/bin/tor-browser" \
            --set MOZ_ENABLE_WAYLAND 1
        '';
      };
    })
    (self: super:
      let
        javaPath = "/tmp/java";
        wrap = drv: name:
          super.symlinkJoin {
            name = drv.name;
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
    (self: super: {
      telegram-desktop = super.telegram-desktop.overrideAttrs (old: {
        patches = let
          baseUrl =
            "https://raw.githubusercontent.com/Layerex/telegram-desktop-patches/2457ef3a2d3ff94dfa4b0a73ea4c51bad4b3f14b/";
        in (old.patches or [ ]) ++ [
          (super.fetchpatch {
            url = baseUrl + "0001-Disable-sponsored-messages.patch";
            sha256 = "sha256-o2Wxyag6hpEDgGm8FU4vs6aCpL9aekazKiNeZPLI9po=";
          })
          (super.fetchpatch {
            url = baseUrl + "0002-Disable-saving-restrictions.patch";
            sha256 = "sha256-sQsyXlvhXSvouPgzYSiRB8ieICo3GDXWH5MaZtBjtqw=";
          })
          (super.fetchpatch {
            url = baseUrl + "0003-Disable-invite-peeking-restrictions.patch";
            sha256 = "sha256-8mJD6LOjz11yfAdY4QPK/AUz9o5W3XdupXxy7kRrbC8=";
          })
          (super.fetchpatch {
            url = baseUrl + "0004-Disable-accounts-limit.patch";
            sha256 = "sha256-PZWCFdGE/TTJ1auG1JXNpnTUko2rCWla6dYKaQNzreg=";
          })
        ];
      });
    })
    # TODO: included in plasma 6.1
    (self: super: {
      kdePackages = super.kdePackages.overrideScope (kde-self: kde-super: {
        kwin = kde-super.kwin.overrideAttrs (old: {
          patches = old.patches ++ [
            (super.fetchpatch {
              url =
                "https://invent.kde.org/plasma/kwin/-/commit/4d6f6223bcdbb0e5fbe65cff47c72d444b532372.patch";
              sha256 = "sha256-0SsRTPLztz3S+6FE09oOurovIaAsp3/JRwNsIwpZAvM=";
            })
          ];
        });
      });
    })
    # TODO: tests fail
    (self: super: {
      fprintd = super.fprintd.overrideAttrs
        (old: { mesonCheckFlags = [ "--no-suite" "fprintd:TestPamFprintd" ]; });
    })
  ];
}
