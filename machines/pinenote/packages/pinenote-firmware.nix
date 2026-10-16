{ pkgs, ... }:

let
  rev = "a137a09be163c5e3fc6e96ba3bfb9eb328394c34";
in
pkgs.stdenvNoCC.mkDerivation rec {
  pname = "pinenote-firmware";
  version = "0.3";

  srcs = [
    (pkgs.fetchurl {
      url = "https://gitlab.com/kernel-firmware/linux-firmware/-/raw/${rev}/cypress/cyfmac43455-sdio.bin";
      sha256 = "sha256-6v+NK20lAbtcR3ujQ5AMdIevkViY6sE7yRszsShdrc4=";
    })
    (pkgs.fetchurl {
      url = "https://gitlab.com/kernel-firmware/linux-firmware/-/raw/${rev}/cypress/cyfmac43455-sdio.clm_blob";
      sha256 = "sha256-j76fwpUuL7qwYqFCweo+JhzXRgR2HhLzBHgbkR30oyg=";
    })
    (pkgs.fetchurl {
      url = "https://gitlab.com/kernel-firmware/linux-firmware/-/raw/${rev}/brcm/brcmfmac43455-sdio.AW-CM256SM.txt";
      sha256 = "sha256-9QNiIHt1NlIcrapZvcKUqa2UaxSfu24zLsKiRcHzT78=";
    })
    (pkgs.fetchurl {
      url = "https://gitlab.com/kernel-firmware/linux-firmware/-/raw/${rev}/LICENSES/LICENCE.cypress";
      sha256 = "sha256-rg22zE2zOUEUjfD2feU+dqd7G1pGsxZe23BAqidQAV8=";
    })
  ];

  dontUnpack = true;

  installPhase = ''
    install -Dm644 ${builtins.elemAt srcs 0} $out/lib/firmware/cypress/cyfmac43455-sdio.bin
    install -Dm644 ${builtins.elemAt srcs 1} $out/lib/firmware/cypress/cyfmac43455-sdio.clm_blob
    install -Dm644 ${builtins.elemAt srcs 2} $out/lib/firmware/brcm/brcmfmac43455-sdio.AW-CM256SM.txt
    install -Dm644 ${builtins.elemAt srcs 3} $out/share/licenses/${pname}/LICENCE.cypress

    ln -s ../cypress/cyfmac43455-sdio.bin \
      $out/lib/firmware/brcm/brcmfmac43455-sdio.pine64,pinenote-v1.2.bin
    ln -s ../cypress/cyfmac43455-sdio.clm_blob \
      $out/lib/firmware/brcm/brcmfmac43455-sdio.clm_blob
    ln -s brcmfmac43455-sdio.AW-CM256SM.txt \
      $out/lib/firmware/brcm/brcmfmac43455-sdio.pine64,pinenote-v1.2.txt
  '';
}
