{ pkgs, ... }:

pkgs.stdenvNoCC.mkDerivation rec {
  pname = "pinenote-firmware";
  version = "0.2";

  srcs = [
    (pkgs.fetchurl {
      url = "https://gitlab.com/kernel-firmware/linux-firmware/-/raw/8d82acd29b5c115f0b75b17907ebce712c6f2e22/cypress/cyfmac43455-sdio.bin";
      sha256 = "sha256-1Aj6qdDVsaL5kS3OpTqwvkghcojjmEBtEX8O2v58Pt0=";
    })
    (pkgs.fetchurl {
      url = "https://github.com/RPi-Distro/firmware-nonfree/raw/3ab8ca8e989a08c4a3cd7f614645311c475b97eb/debian/config/brcm80211/cypress/cyfmac43455-sdio.clm_blob";
      sha256 = "sha256-mCOELK6fuaXdHl+zH1lVFux97uNBNUvvMLswJu7inME=";
    })
    (pkgs.fetchurl {
      url = "https://gitlab.com/kernel-firmware/linux-firmware/-/raw/8d82acd29b5c115f0b75b17907ebce712c6f2e22/brcm/brcmfmac43455-sdio.AW-CM256SM.txt";
      sha256 = "sha256-9QNiIHt1NlIcrapZvcKUqa2UaxSfu24zLsKiRcHzT78=";
    })
    (pkgs.fetchurl {
      url = "https://gitlab.com/kernel-firmware/linux-firmware/-/raw/main/LICENCE.cypress";
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
