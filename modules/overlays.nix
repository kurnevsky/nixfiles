{ pkgs, ... }:

{
  # system.replaceRuntimeDependencies can be used to make fast fixes
  nixpkgs.overlays = [
    (self: super: {
      vaapiIntel = super.vaapiIntel.override { enableHybridCodec = true; };
      deadbeef = super.deadbeef.override { wavpackSupport = true; };
      deadbeef-with-plugins = super.deadbeef-with-plugins.override {
        plugins = with super.deadbeefPlugins; [ mpris2 statusnotifier ];
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
          tdlib-purple
          (pkgs.callPackage ./pidgin-indicator.nix { })
        ];
      };
      ungoogled-chromium-wayland = super.ungoogled-chromium.override {
        commandLineArgs = "--ozone-platform-hint=auto";
      };
      isync = let
        cyrus_sasl_xoauth2 = pkgs.stdenv.mkDerivation {
          pname = "cyrus-sasl-xoauth2";
          version = "1";

          src = pkgs.fetchFromGitHub {
            owner = "moriyoshi";
            repo = "cyrus-sasl-xoauth2";
            rev = "36aabca54fd65c8fa7a707cb4936751599967904";
            sha256 = "sha256-OlmHuME9idC0fWMzT4kY+YQ43GGch53snDq3w5v/cgk=";
          };

          postPatch = ''
            touch AUTHORS
            touch ChangeLog
            touch NEWS
          '';

          installPhase = ''
            mkdir -p $out/lib/sasl2
            cp .libs/libxoauth2.so $out/lib/sasl2
          '';

          nativeBuildInputs = [ pkgs.autoreconfHook ];
          buildInputs = [ pkgs.cyrus_sasl ];
        };
      in pkgs.symlinkJoin {
        name = "isync";
        paths = [
          (pkgs.writeShellScriptBin "mbsync" ''
            export SASL_PATH=${super.cyrus_sasl.out}/lib/sasl2:${cyrus_sasl_xoauth2}/lib/sasl2
            exec ${super.isync}/bin/mbsync "$@"
          '')
          super.isync
        ];
      };
      cloud-mdir-sync = (pkgs.callPackage ./cloud-mdir-sync.nix { });
    })
    (self: super: {
      plasma5Packages = super.plasma5Packages.overrideScope' (self: super: {
        plasma5 = super.plasma5 // {
          kwin = super.plasma5.kwin.overrideAttrs (old: {
            postPatch = old.postPatch + ''
              substituteInPlace src/effects/slide/slide.cpp \
                --replace \
                  'const qreal springConstant = 200.0 / effects->animationTimeFactor();' \
                  'const qreal springConstant = 500.0 / effects->animationTimeFactor();'
            '';
          });
        };
      });
    })
    (self: super: {
      globalprotect-openconnect = super.globalprotect-openconnect.overrideAttrs
        (old: rec {
          version = "1.4.7";
          src = super.fetchFromGitHub {
            owner = "yuezk";
            repo = "GlobalProtect-openconnect";
            fetchSubmodules = true;
            rev = "v${version}";
            sha256 = "sha256-MNH6zizPX3tcFsEPC5w0lr48KlV578kYe+f5v8Qc5FY=";
          };
        });
    })
  ];
}
