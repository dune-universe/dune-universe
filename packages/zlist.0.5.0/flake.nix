{
  description = "Lazy lists for OCaml.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-20.09";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };
    in
    {
      devShell.${system} = pkgs.mkShell {
        buildInputs = [
          pkgs.ocamlformat
        ] ++ (with pkgs.ocamlPackages; [
          dune_2
          findlib
          merlin
          ocaml
          odoc
          utop
        ]);
      };

      packages.${system} = {
        zlist = pkgs.ocamlPackages.buildDunePackage {
          useDune2 = true;
          pname = "zlist";
          src = self;
          version = "0.5.0";
          buildInputs = [ ];
        };
      };

      defaultPackage.${system} = self.packages.${system}.zlist;
    };
}
