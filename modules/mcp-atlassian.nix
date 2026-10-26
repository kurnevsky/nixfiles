{
  lib,
  python3,
  fetchFromGitHub,
}:

let
  # mcp-atlassian requires mcp<2 while nixpkgs has mcp 2.x
  python = python3.override {
    self = python;
    packageOverrides = _self: super: {
      mcp = super.mcp.overridePythonAttrs (_: rec {
        version = "1.29.1";
        src = fetchFromGitHub {
          owner = "modelcontextprotocol";
          repo = "python-sdk";
          tag = "v${version}";
          hash = "sha256-x5cm1QojKp7K1mKc9ejazzJzBhIkFBif6/A+hEryYKo=";
        };
        dependencies = with super; [
          anyio
          httpx
          httpx-sse
          jsonschema
          pydantic
          pydantic-settings
          pyjwt
          python-multipart
          sse-starlette
          starlette
          typing-extensions
          typing-inspection
          uvicorn
        ]
        ++ pyjwt.optional-dependencies.crypto;
        doCheck = false;
      });
    };
  };
  python3Packages = python.pkgs;

  markdown-to-confluence = python3Packages.buildPythonPackage (finalAttrs: {
    pname = "markdown-to-confluence";
    version = "0.6.4";
    pyproject = true;

    src = fetchFromGitHub {
      owner = "hunyadi";
      repo = "md2conf";
      tag = finalAttrs.version;
      hash = "sha256-+jbCJKOivJWuxfgLT7sUEJWunfG2S0i6qaC04MQojik=";
    };

    build-system = with python3Packages; [ setuptools ];

    # nixpkgs has slightly older versions that work fine
    pythonRelaxDeps = [
      "cattrs"
      "orjson"
      "pymdown-extensions"
    ];

    dependencies = with python3Packages; [
      cattrs
      lxml
      markdown
      orjson
      pathspec
      pymdown-extensions
      pyyaml
      requests
      truststore
    ];

    pythonImportsCheck = [ "md2conf" ];

    meta = {
      description = "Publish Markdown files to Confluence wiki";
      homepage = "https://github.com/hunyadi/md2conf";
      license = lib.licenses.mit;
      mainProgram = "md2conf";
    };
  });
in

python3Packages.buildPythonApplication (finalAttrs: {
  pname = "mcp-atlassian";
  version = "0.23.1";
  pyproject = true;

  src = fetchFromGitHub {
    owner = "sooperset";
    repo = "mcp-atlassian";
    tag = "v${finalAttrs.version}";
    hash = "sha256-zbBsEBEfSvsS7dTbbvx2HG2GQhxGtvwVpYf4JPZVCKw=";
  };

  # it's determined from the git history that is absent in the tarball
  env.UV_DYNAMIC_VERSIONING_BYPASS = finalAttrs.version;

  build-system = with python3Packages; [
    hatchling
    uv-dynamic-versioning
  ];

  # type stubs that aren't needed at runtime
  pythonRemoveDeps = [
    "types-cachetools"
    "types-python-dateutil"
  ];

  pythonRelaxDeps = [ "fakeredis" ];

  dependencies =
    with python3Packages;
    [
      anyio
      atlassian-python-api
      beautifulsoup4
      cachetools
      click
      fakeredis
      fastmcp
      httpx
      markdown
      markdownify
      mcp
      pydantic
      pysocks
      python-dateutil
      python-dotenv
      requests
      starlette
      thefuzz
      trio
      truststore
      unidecode
      urllib3
      uvicorn
    ]
    ++ [ markdown-to-confluence ];

  pythonImportsCheck = [ "mcp_atlassian" ];

  meta = {
    description = "MCP server for Atlassian products (Confluence and Jira)";
    homepage = "https://github.com/sooperset/mcp-atlassian";
    license = lib.licenses.mit;
    mainProgram = "mcp-atlassian";
  };
})
