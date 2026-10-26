{ inputs, ... }:

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
      firefox = super.firefox.override {
        nativeMessagingHosts = [
          super.keepassxc
          super.kdePackages.plasma-browser-integration
        ];
      };
      mpv = super.mpv.override {
        mpv-unwrapped = super.mpv-unwrapped.override {
          vapoursynthSupport = true;
          # --hwdec=auto-copy --vf=vapoursynth=rife.vpy:buffered-frames=8:concurrent-frames=32 --hr-seek-framedrop=no --video-sync=display-resample
          vapoursynth = super.vapoursynth.withPlugins [
            (super.callPackage ./rife-ncnn.nix { })
            (super.callPackage ./vs-miscfilters-obsolete.nix { })
          ];
        };
        extraMakeWrapperArgs = [
          # Add paths to required libraries
          "--prefix"
          "LD_LIBRARY_PATH"
          ":"
          "/run/opengl-driver/lib:${super.lib.makeLibraryPath [ super.ocl-icd ]}"
        ];
        scripts = with super.mpvScripts; [ mpris ];
      };
      _7zz = super._7zz.override { enableUnfree = true; };
      p7zip = super.p7zip.override { enableUnfree = true; };
      # patchelf 0.19 breaks the binary when setting the interpreter before
      # rpath: https://github.com/NixOS/patchelf/issues/674
      # TODO: remove after https://github.com/NixOS/patchelf/pull/668
      purescript = super.purescript.overrideAttrs (old: {
        installPhase =
          builtins.replaceStrings
            [ "patchelf --interpreter ${super.stdenv.cc.bintools.dynamicLinker} --set-rpath ${old.libPath} $PURS" ]
            [
              ''
                patchelf --set-rpath ${old.libPath} $PURS
                patchelf --set-interpreter ${super.stdenv.cc.bintools.dynamicLinker} $PURS
              ''
            ]
            old.installPhase;
      });
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
    })
    (self: super: {
      tor-browser = self.symlinkJoin {
        name = "tor-browser";
        paths = [
          (self.writeShellScriptBin "tor-browser" ''
            ${self.coreutils}/bin/env \
              TOR_SKIP_LAUNCH=1 \
              TOR_SOCKS_PORT=9050 \
              TOR_CONTROL_PORT=9051 \
              TOR_CONTROL_COOKIE_AUTH_FILE=/var/lib/tor/control_auth_cookie \
              ${super.tor-browser}/bin/tor-browser "$@"
          '')
          super.tor-browser
        ];
      };
    })
    (_self: super: {
      llama-cpp = super.llama-cpp.overrideAttrs (old: {
        version = "0";
        src = old.src.overrideAttrs {
          rev = "9d1878a4354e743529fbe09e7350a957f5e39e9a";
          hash = "sha256-7oHpV1DaBIK+Zenm+Dkkqmn5Qt2xHdZLOsdvaoGthVE=";
        };
        npmDepsHash = "sha256-hqYzdp1FwPhYkBP278fQXgY9NCKB6Q3P6Gko7BfP0m0=";
        cmakeFlags = old.cmakeFlags ++ [
          (super.lib.cmakeFeature "GGML_SCHED_MAX_COPIES" "1")
        ];
        # Patches from nixpkgs are already included in this revision.
        patches = [ ];
        disabledTests = old.disabledTests ++ [
          # Requires the network-dependent test-download-model fixture.
          "test-llama-context-model"
        ];
      });
      # Keep stock whisper-cpp in ffmpeg-full so it is fetched from cache.
      ffmpeg-full =
        let
          noGpu = {
            cudaSupport = false;
            rocmSupport = false;
          };
        in
        super.ffmpeg-full.override {
          whisper-cpp = super.whisper-cpp.override (noGpu // { llama-cpp = super.llama-cpp.override noGpu; });
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
        super.lib.mapAttrs (_: createChromiumExtension) (
          builtins.fromJSON (builtins.readFile ./chromium-extensions.json)
        );
    })
  ];
}
