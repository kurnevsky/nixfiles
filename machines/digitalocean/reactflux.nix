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
  version = "2025.07.30";

  src = fetchFromGitHub {
    owner = "electh";
    repo = "ReactFlux";
    tag = "v${finalAttrs.version}";
    hash = "sha256-Ew+SKdkQX8h/TL1i9O6+WqeVqDfYBpvGUO0n3s5YbT0=";
  };

  nativeBuildInputs = [
    nodejs
    pnpm.configHook
  ];

  pnpmDeps = pnpm.fetchDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 1;
    hash = "sha256-DYAPLivs1eQZH5yVM4+FAFQIJzSs7sLMkh3+E0V3O7I=";
  };

  postPatch = ''
    substituteInPlace src/scripts/version-info.js \
      --replace-fail 'execSync("git rev-parse --short HEAD").toString().trim()' '"-"' \
      --replace-fail 'execSync("git log -1 --format=%cd --date=iso").toString().trim()' '"${finalAttrs.version}"'
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
