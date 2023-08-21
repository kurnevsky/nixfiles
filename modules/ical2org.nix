{ stdenv, fetchFromGitHub, ... }:

stdenv.mkDerivation {
  name = "ical2org.awk";
  src = fetchFromGitHub {
    owner = "msherry";
    repo = "ical2org";
    rev = "07a7bbb3754d6b2d0ed60449b54977fd84e4949a";
    hash = "sha256-A/lmBxYBjzgUZ4Z9oumBKm+HsoTNjdkf+BsDFIGRb78=";
  };
  installPhase = "mkdir -p $out/bin && cp ical2org.awk $out/bin/ && chmod +x $out/bin/ical2org.awk";
}
