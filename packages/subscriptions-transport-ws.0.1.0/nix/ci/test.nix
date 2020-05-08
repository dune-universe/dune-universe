{ ocamlVersion }:

let
  pkgs = import ../sources.nix { inherit ocamlVersion; };
  inherit (pkgs) lib stdenv fetchTarball ocamlPackages;

  subscriptions-ws-pkgs =  (import ./.. {
    inherit pkgs;
  });

  in
  stdenv.mkDerivation {
    name = "subscriptions-transport-ws-test";
    src = ./../..;
    dontBuild = true;
    installPhase = ''
      touch $out
    '';
    buildInputs =
    (lib.attrValues subscriptions-ws-pkgs) ++
    (with ocamlPackages; [ ocaml dune findlib pkgs.ocamlformat ]);
    doCheck = true;
    checkPhase = ''
      # Check code is formatted with OCamlformat
      dune build @fmt
    '';
  }
