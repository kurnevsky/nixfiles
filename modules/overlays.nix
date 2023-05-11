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
      firefox = super.firefox.override {
        extraNativeMessagingHosts = [ super.passff-host ];
      };
      firefox-wayland = super.firefox-wayland.override {
        extraNativeMessagingHosts = [ super.passff-host ];
      };
      mpv = super.mpv.override { scripts = with pkgs.mpvScripts; [ mpris ]; };
      p7zip = super.p7zip.override { enableUnfree = true; };
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
    (self: super: {
      telegram-desktop = super.telegram-desktop.overrideAttrs (old: {
        patches = let
          baseUrl =
            "https://raw.githubusercontent.com/Layerex/telegram-desktop-patches/cfcced0bf6226fe273eb4fee0aaa625082ab041a/";
        in (old.patches or [ ]) ++ [
          (super.fetchpatch {
            url = baseUrl + "0001-Disable-sponsored-messages.patch";
            sha256 = "sha256-ZHdLxiRfRIFU2RU0v03DpCjW/ZZDTicJ2zRCGWz+dBc=";
          })
          (super.fetchpatch {
            url = baseUrl + "0002-Disable-saving-restrictions.patch";
            sha256 = "sha256-lKO8FVIlXjvPC+xdVgWw6DeSRkQ5VGp/Yf6ByjScJjk=";
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
    (self: super: { wine-ge = pkgs.callPackage ./wine-ge.nix { }; })
  ];
}
