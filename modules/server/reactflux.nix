{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  fetchzip,

  nodejs,
  pnpm,
  pnpmConfigHook,
  fetchPnpmDeps,

  baseurl,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "ReactFlux";
  version = "2026.10.31";

  src = fetchFromGitHub {
    owner = "electh";
    repo = "ReactFlux";
    tag = "v${finalAttrs.version}";
    hash = "sha256-kE/hLJjHutTJNHA9x11d3cfBwcclMfPU0RfVcRpkb5A=";
  };

  nativeBuildInputs = [
    nodejs
    pnpmConfigHook
    pnpm
  ];

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 4;
    hash = "sha256-QP6NgpdOVU/zbQYc+3ljVPUaOlgqVNAFyjz2q3pkMPA=";
  };

  env = {
    SOURCE_COMMIT = "-";
    SOURCE_COMMIT_DATE = lib.replaceStrings [ "." ] [ "-" ] finalAttrs.version;
  };

  postPatch = ''
    substituteInPlace src/AppNotifications.jsx \
      --replace-fail 'useVersionCheck()' '{ hasUpdate: false, dismissUpdate: () => {} }'
  '';

  buildPhase = ''
    runHook preBuild

    pnpm run build --base=${baseurl}

    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    cp -r build $out

    runHook postInstall
  '';

  meta = {
    description = "A Simple but Powerful RSS Reader for Miniflux";
    homepage = "https://github.com/electh/ReactFlux";
    license = lib.licenses.mit;
    maintainers = with lib.maintainers; [ kurnevsky ];
  };
})
