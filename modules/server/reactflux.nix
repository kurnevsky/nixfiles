{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
  fetchzip,

  nodejs,
  pnpm,

  baseurl,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "ReactFlux";
  version = "2025.12.16";

  src = fetchFromGitHub {
    owner = "electh";
    repo = "ReactFlux";
    tag = "v${finalAttrs.version}";
    hash = "sha256-0Vg1fhJL/d3KZ2O2vnS8hpZ8Tfw0CsRWnOXmckLXeCM=";
  };

  nativeBuildInputs = [
    nodejs
    pnpm.configHook
  ];

  pnpmDeps = pnpm.fetchDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 1;
    hash = "sha256-7HaEDEiFBPjMtx+OHucCqQ7SaE0RAhYPLDssQbwsDoQ=";
  };

  postPatch = ''
    substituteInPlace src/scripts/version-info.js \
      --replace-fail 'execSync("git rev-parse --short HEAD").toString().trim()' '"-"' \
      --replace-fail 'execSync("git log -1 --format=%cd --date=iso").toString().trim()' '"${finalAttrs.version}"'

    substituteInPlace src/App.jsx \
      --replace-fail 'useVersionCheck()' '{ hasUpdate: false }'
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
