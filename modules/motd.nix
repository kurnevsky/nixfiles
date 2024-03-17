name:

let
  messages = [
    # Dante, Divine Comedy
    "Abandon all hope, ye who enter here."
    # William Shakespeare, The Tempest
    "Hell is empty, and all the devils are here."
    # Douglas Adams, The Hitchhiker’s Guide to the Galaxy
    "Beware of the leopard."
    # The Hunt–Lenox Globe
    "Here be dragons."
    # Randoms
    "Tread lightly, for you are treading on sacred ground."
    "Cross the threshold, but remember that you leave your innocence behind."
    "Forsake your expectations, for this place defies understanding."
    "Beware of the lion lurking in the shadows."
    "Flee from this place, for all is lost within."
    "Within these boundaries lie untamed terrors."
    "Venture forth, but abandon your illusions at the gate."
    "Those who dare to cross shall face trials unimaginable."
    "Do not seek to unravel mysteries that were never meant for mortal eyes."
    "Do not venture too far into the abyss, lest it swallow you whole."
  ];
  length = builtins.length messages;
  hash = builtins.hashString "md5" name;
  hex = builtins.substring 0 8 hash;
  number = (builtins.fromTOML "n = 0x${hex}").n;
  index = number - (number / length) * length;
in builtins.elemAt messages index
