{ pkgs ? import ./sources.nix { inherit ocamlVersion; }
, ocamlVersion ? "4_10"
}:

let
  inherit (pkgs) lib stdenv ocamlPackages;
in

  with ocamlPackages;

  {
    subscriptions-transport-ws = buildDunePackage {
      pname = "subscriptions-transport-ws";
      version = "0.0.1-dev";
      useDune2 = true;
      src = lib.gitignoreSource ./..;
      propagatedBuildInputs = [
        websocketaf
        graphql
      ];
    };
  }
