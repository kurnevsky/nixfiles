# https://github.com/NixOS/nixpkgs/pull/392478

{ nodeEnv, fetchurl, ... }:

nodeEnv.buildNodePackage {
  name = "prettier-plugin-toml";
  packageName = "prettier-plugin-toml";
  version = "2.0.2";
  src = fetchurl {
    url = "https://registry.npmjs.org/prettier-plugin-toml/-/prettier-plugin-toml-2.0.2.tgz";
    sha512 = "tUIIhyfdVX5DMsLGKX/2qaEwi3W48OkUSR7XC91PRI5jFzhexmaYWkrSP1Xh/eWUcEc0TVMQenM3lB09xLQstQ==";
  };
  dependencies = [
    {
      name = "_at_taplo_slash_core";
      packageName = "@taplo/core";
      version = "0.1.1";
      src = fetchurl {
        url = "https://registry.npmjs.org/@taplo/core/-/core-0.1.1.tgz";
        sha512 = "BG/zLGf5wiNXGEVPvUAAX/4ilB3PwDUY2o0MV0y47mZbDZ9ad9UK/cIQsILat3bqbPJsALVbU6k3cskNZ3vAQg==";
      };
    }
    {
      name = "_at_taplo_slash_lib";
      packageName = "@taplo/lib";
      version = "0.4.0-alpha.2";
      src = fetchurl {
        url = "https://registry.npmjs.org/@taplo/lib/-/lib-0.4.0-alpha.2.tgz";
        sha512 = "DV/Re3DPVY+BhBtLZ3dmP4mP6YMLSsgq9qGLXwOV38lvNF/fBlgvQswzlXmzCEefL/3q2eMoefZpOI/+GLuCNA==";
      };
    }
    {
      name = "prettier";
      packageName = "prettier";
      version = "3.5.3";
      src = fetchurl {
        url = "https://registry.npmjs.org/prettier/-/prettier-3.5.3.tgz";
        sha512 = "QQtaxnoDJeAkDvDKWCLiwIXkTgRhwYDEQCghU9Z6q03iyek/rxRh/2lC3HB7P8sWT2xC/y5JDctPLBIGzHKbhw==";
      };
    }
  ];
  meta = {
    description = "An opinionated `toml` formatter plugin for Prettier";
    homepage = "https://github.com/un-ts/prettier/tree/master/packages/toml";
    license = "MIT";
  };
  production = true;
  bypassCache = true;
  reconstructLock = true;
}
