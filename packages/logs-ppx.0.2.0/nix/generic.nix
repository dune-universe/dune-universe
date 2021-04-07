{ pkgs, stdenv, lib, ocamlPackages, static ? false, doCheck }:

with ocamlPackages;

rec {
  oidc = buildDunePackage {
    pname = "logs-ppx";
    version = "0.2.0-dev";

    src = lib.filterGitSource {
      src = ./..;
      dirs = [ "src" "test" ];
      files = [ "dune-project" "logs-ppx.opam" ];
    };

    useDune2 = true;

    buildInputs = [
      logs
    ];

    propagatedBuildInputs = [
      ppxlib
    ];

    inherit doCheck;

    meta = {
      description = "Base functions and types to work with OpenID Connect.";
      license = stdenv.lib.licenses.bsd3;
    };
  };
}
