open Base
open Biotk
open Biotk_croquis
open Gg.Color

let tree =
  let open Phylo_tree_draw in
  let leaf = leaf ~style:`italic in
  node [
    branch 0.5 (leaf "Pan troglodytes") ;
    branch 2.5 (
      node ~tag:red [
        branch ~col:red 1. (Phylo_tree_draw.leaf ~col:red "Mus musculus");
        branch ~col:blue 1.5 (leaf ~col:blue "Rattus norvegicus");
      ]
    )
  ]
  |> draw_tree

let pssm =
  let module PSSM = Profile_matrix.DNA in
  [|
    [| 0.654 ; 0.045 ; 0.262 ; 0.039 |] ;
    [| 0.019 ; 0.01  ; 0.935 ; 0.036 |] ;
    [| 0.042 ; 0.013 ; 0.673 ; 0.272 |] ;
    [| 0.013 ; 0.074 ; 0.133 ; 0.78  |] ;
    [| 0.01  ; 0.819 ; 0.113 ; 0.058 |] ;
    [| 0.893 ; 0.01  ; 0.068 ; 0.029 |]
  |]
  |> PSSM.of_array
  |> Stdlib.Option.get
  |> PSSM.draw

let picture =
  Croquis.Picture.vstack [
    tree ;
    pssm ;
  ]
  |> Croquis.Layout.simple

let () =
  Croquis.Layout.render `pdf picture (`File "croquis_demo.pdf")
