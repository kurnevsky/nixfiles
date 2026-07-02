{ pkgs, ... }:

let
  inherit (pkgs) lib;
  pythonEnv = pkgs.python3.withPackages (
    ps: with ps; [
      numpy
      pydbus
      dbus-next
      i3ipc
    ]
  );
in
pkgs.stdenvNoCC.mkDerivation {
  name = "hrdl-utils";
  meta = with lib; {
    description = "Various scripts from hrdls pinenote-dist repo";
    license = licenses.gpl3Only;
    homepage = "https://git.sr.ht/~hrdl/pinenote-dist";
  };

  src = pkgs.fetchFromSourcehut {
    owner = "~hrdl";
    repo = "pinenote-dist";
    rev = "3e228dbf3fe26f74ab7ec3a971adc477d9c0bfe6";
    sha256 = "sha256-SWHEK6Clv0H2hdRxi4DyLKVDxuPeIpzsZGaITuPgg+s=";
  };

  nativeBuildInputs = with pkgs; [
    makeWrapper
    wrapGAppsNoGuiHook
    gobject-introspection
  ];
  buildInputs = with pkgs; [
    pythonEnv
    hexdump
  ];

  patchPhase = ''
    substituteInPlace bin/waveform_extract.sh --replace-fail '/usr/lib/' '/lib/'
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp bin/read_file.py $out/bin/
    cp bin/wbf_to_custom.py $out/bin/
    cp bin/waveform_extract.sh $out/bin/
    cp bin/rockchip_ebc_custom_ioctl.py $out/bin/
    cp bin/sway_dbus_integration.py $out/bin/
    cp bin/toggle_onscreen_keyboard.py $out/bin/

    wrapProgram $out/bin/waveform_extract.sh \
      --prefix PATH : ${
        lib.makeBinPath (
          with pkgs;
          [
            coreutils
            hexdump
          ]
        )
      }
  '';
}
