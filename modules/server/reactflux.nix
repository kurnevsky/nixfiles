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
  version = "2026.08.22";

  src = fetchFromGitHub {
    owner = "electh";
    repo = "ReactFlux";
    tag = "v${finalAttrs.version}";
    hash = "sha256-AGFKdeNsGlQDF1UWZ7VPD39P+UD2iYtBwVnQK3WWnTY=";
  };

  nativeBuildInputs = [
    nodejs
    pnpmConfigHook
    pnpm
  ];

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 4;
    hash = "sha256-Nazo2zo9t1HHzPV+5OKkGEsbLJDrUfKqrH90I9lmJXU=";
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
