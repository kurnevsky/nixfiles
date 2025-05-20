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
    (_self: super: {
      # https://chrome.google.com/webstore/detail/{id}
      chromium-extensions =
        let
          createChromiumExtensionFor =
            browserVersion:
            {
              id,
              sha256,
              version,
            }:
            {
              inherit id;
              crxPath = builtins.fetchurl {
                url = "https://clients2.google.com/service/update2/crx?response=redirect&acceptformat=crx2,crx3&prodversion=${browserVersion}&x=id%3D${id}%26installsource%3Dondemand%26uc";
                name = "${id}.crx";
                inherit sha256;
              };
              inherit version;
            };
          createChromiumExtension = createChromiumExtensionFor (
            super.lib.versions.major super.ungoogled-chromium.version
          );
        in
        {
          dark-reader = createChromiumExtension {
            id = "eimadpbcbfnmbkopoojfekhnkhdbieeh";
            sha256 = "sha256:1lc5ny57rdpqzx07gwj44pl3m9mg9nq6p5kjchrmjjh4hkk5wsny";
            version = "4.9.106";
          };
          decentraleyes = createChromiumExtension {
            id = "ldpochfccmkkmhdbclfhpagapcfdljkj";
            sha256 = "sha256:198k1hyzf3a1yz4chnx095rwqa15hkcck4ir6xs6ps29qgqw8ili";
            version = "3.0.0";
          };
          plasma-integration = createChromiumExtension {
            id = "cimiefiiaegbelhefglklhhakcgmhkai";
            sha256 = "sha256:0nix7ilqdk12fx5afqn1dswq9ana096d440zr7p8y9ijvjgd9vlv";
            version = "2.0";
          };
          privacy-badger = createChromiumExtension {
            id = "pkehgijcmpdhfbdbbnkijodmdjhbjlgp";
            sha256 = "sha256:0kx6pgk3m9mdqwnh2lxw1cid9csv6xvchdw55svimrl5ky2dqlzx";
            version = "2025.5.14";
          };
          sponsorblock = createChromiumExtension {
            id = "mnjggcdmjocbbbhaepdhchncahnbgone";
            sha256 = "sha256:1vs5bqrkqkssmn32iblp4xr5pqbh941rc0zl8qmyz7khhp318aw1";
            version = "5.12.2";
          };
          ublock-origin-lite = createChromiumExtension {
            id = "ddkjiahejlhfcafbddmgiahcphecmpfh";
            sha256 = "sha256:0ynlq33134ajppfhb9hx9cqsxqqm4l492fnrg3ssq91m0sjqvzfa";
            version = "2025.512.1008";
          };
          find-plus = createChromiumExtension {
            id = "fddffkdncgkkdjobemgbpojjeffmmofb";
            sha256 = "sha256:03yhzc8yhcx2shm34pm54fhrnhb3hfccgnk2q6qkhysk9ylpywq3";
            version = "2.2.2";
          };
          i-still-dont-care-about-cookies = createChromiumExtension {
            id = "edibdbjcniadpccecjdfdjjppcpchdlm";
            sha256 = "sha256:11k7cxcjafs8ziaxl4bilbfwbgl2yf1p6v1bvwszadcr14xyvgsj";
            version = "1.1.4";
          };
          refined-github = createChromiumExtension {
            id = "hlepfoohegkhhmjieoechaddaejaokhf";
            sha256 = "sha256:08chhqp83xdzhv5fd6xvgh61k17srfij3yj2hm6851f2qvria8zw";
            version = "25.4.8";
          };
        };
    })
  ];
}
