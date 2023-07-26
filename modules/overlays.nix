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
      mpv = super.mpv.override { scripts = with pkgs.mpvScripts; [ mpris ]; };
      p7zip = super.p7zip.override { enableUnfree = true; };
      isync = pkgs.symlinkJoin {
        name = "isync";
        paths = [
          (pkgs.writeShellScriptBin "mbsync" ''
            export SASL_PATH=${super.cyrus_sasl.out}/lib/sasl2:${pkgs.cyrus-sasl-xoauth2}/lib/sasl2
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
    (self: super: {
      gnupg_patched = super.gnupg.overrideAttrs (old: {
        patches = old.patches ++ [
          (super.fetchpatch {
            # https://dev.gnupg.org/T6481
            url =
              "https://files.gnupg.net/file/data/iapdg4omgofpqs5c6gd5/PHID-FILE-pym22ajak2knrci6sc2h/file";
            sha256 = "sha256-irxxBBO0MqHaUj0a0qZ8yfhKp4wXRdQErj3eD/ZF66I=";
          })
        ];
      });
    })
  ];
}
