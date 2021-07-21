{ lib, stdenv, pidgin, fetchFromGitHub, autoreconfHook, intltool
, libappindicator-gtk2 }:

stdenv.mkDerivation rec {
  pname = "pidgin-indicator";
  version = "1.0.1";

  src = fetchFromGitHub {
    owner = "philipl";
    repo = "pidgin-indicator";
    rev = version;
    sha256 = "sha256-CdA/aUu+CmCRbVBKpJGydicqFQa/rEsLWS3MBKlH2/M=";
  };

  makeFlags = [ "PURPLE_PLUGIN_DIR=$(out)/lib/pidgin" ];

  postPatch = [''
    substituteInPlace configure.ac \
      --replace 'PIDGIN_DATADIR="$datadir"' 'PIDGIN_DATADIR="${pidgin}"/share'
  ''];

  nativeBuildInputs = [ autoreconfHook ];
  buildInputs = [ pidgin intltool libappindicator-gtk2 ];

  meta = with lib; {
    homepage = "https://github.com/philipl/pidgin-indicator";
    description = "AppIndicator/KStatusNotifierItem Plugin for Pidgin";
    license = licenses.gpl2;
    platforms = platforms.linux;
  };
}
