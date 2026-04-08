{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:

buildNpmPackage (finalAttrs: {
  pname = "firefox-devtools-mcp";
  version = "0.9.1";

  src = fetchFromGitHub {
    owner = "mozilla";
    repo = "firefox-devtools-mcp";
    tag = finalAttrs.version;
    hash = "sha256-Az4okHS6XOuKhJ3wkQNRy7ZUyfnqy7NJGq/dPNS9Zs0=";
  };

  npmDepsHash = "sha256-xRmflTT/XFxONzrzcyZa1n9rMdqZ2NAmhMPksNBOEa0=";

  meta = {
    description = "Model Context Protocol server for Firefox DevTools";
    homepage = "https://github.com/mozilla/firefox-devtools-mcp";
    license = lib.licenses.mit;
  };
})
