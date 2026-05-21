{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage (finalAttrs: {
  pname = "anki-mcp-server";
  version = "0.18.5";

  src = fetchFromGitHub {
    owner = "ankimcp";
    repo = "anki-mcp-server";
    tag = "v${finalAttrs.version}";
    hash = "sha256-bmKzfQAgTcY1+Upy/M/SYkNCWhFpK6ZoRhKflUyQr3w=";
  };

  npmDepsHash = "sha256-DkPMZ1PgeQ5eHSQtu/CiC7WrKUDt6ndwHjA68bogTxU=";

  meta = {
    description = "A Model Context Protocol (MCP) server that enables AI assistants to interact with Anki";
    homepage = "https://github.com/ankimcp/anki-mcp-server";
    license = lib.licenses.agpl3Plus;
  };
})
