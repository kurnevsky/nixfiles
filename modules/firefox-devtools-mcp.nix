{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage (finalAttrs: {
  pname = "firefox-devtools-mcp";
  version = "0.9.3";

  src = fetchFromGitHub {
    owner = "mozilla";
    repo = "firefox-devtools-mcp";
    tag = finalAttrs.version;
    hash = "sha256-GPssns4WyWoRJDBxbv01UZ/XQPRZZWASuHPLtbtdjq0=";
  };

  npmDepsHash = "sha256-zK0L4q77ONyeK5xh5JaehvCh/zv9D5OuesP3PHQqT2o=";

  meta = {
    description = "Model Context Protocol server for Firefox DevTools";
    homepage = "https://github.com/mozilla/firefox-devtools-mcp";
    license = lib.licenses.mit;
  };
})
