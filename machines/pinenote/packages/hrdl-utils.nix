{ pkgs, ... }:

let
  inherit (pkgs) lib;
  pythonEnv = pkgs.python3.withPackages (
    ps: with ps; [
      numpy
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
    rev = "0d463616813e678693e741e5c2cea056f4270718";
    sha256 = "sha256-9T+xe252nqMK63KlrFawb1KyQ+JXFVnIfgBI0ZdWrjQ=";
  };

  nativeBuildInputs = with pkgs; [ makeWrapper ];
  buildInputs = with pkgs; [
    pythonEnv
    hexdump
  ];

  patchPhase = ''
    sed -i 's/\/usr\/lib\//\/lib\//g' bin/waveform_extract.sh
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp bin/read_file.py $out/bin/
    cp bin/wbf_to_custom.py $out/bin/
    cp bin/waveform_extract.sh $out/bin/
    cp bin/rockchip_ebc_custom_ioctl.py $out/bin/

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
