{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage (finalAttrs: {
  pname = "anki-mcp-server";
  version = "0.27.0";

  src = fetchFromGitHub {
    owner = "ankimcp";
    repo = "anki-mcp-server";
    tag = "v${finalAttrs.version}";
    hash = "sha256-0ez5QEp0ltq0gHZc+TedPjClZBCZE3xrEUkmXoVEapg=";
  };

  npmDepsHash = "sha256-rIgn9zpJzsoCbS2mrqQk36Cx4nsrfyOWXN3zLeIyk3E=";

  meta = {
    description = "A Model Context Protocol (MCP) server that enables AI assistants to interact with Anki";
    homepage = "https://github.com/ankimcp/anki-mcp-server";
    license = lib.licenses.agpl3Plus;
  };
})
