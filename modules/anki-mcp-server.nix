{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage (finalAttrs: {
  pname = "anki-mcp-server";
  version = "0.26.0";

  src = fetchFromGitHub {
    owner = "ankimcp";
    repo = "anki-mcp-server";
    tag = "v${finalAttrs.version}";
    hash = "sha256-rfyqa4LD6H4N9MNtIgoDOCs5MDXoPX3GR1Drjzb7pD8=";
  };

  npmDepsHash = "sha256-n/C4IgRK1qVfa5A3cBEfSgX5uUUJvhte+/X2YP9ayhU=";

  meta = {
    description = "A Model Context Protocol (MCP) server that enables AI assistants to interact with Anki";
    homepage = "https://github.com/ankimcp/anki-mcp-server";
    license = lib.licenses.agpl3Plus;
  };
})
