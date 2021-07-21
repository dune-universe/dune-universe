open Gg
open Biotk_croquis

type tree =
  | Leaf of {
      text : string ;
      style : [ `normal | `bold | `italic ] ;
      color : Color.t
    }
  | Node of {
      children : branch list ;
      tag : Color.t option
    }
and branch = Branch of {
    length : float ;
    tip : tree ;
    color : Color.t ;
  }

val leaf :
  ?style:[`normal | `bold | `italic] ->
  ?col:Color.t ->
  string ->
  tree

val bnode :
  ?tag:Color.t ->
  branch -> branch -> tree

val node :
  ?tag:Color.t ->
  branch list ->
  tree

val branch : ?col:Color.t -> float -> tree -> branch

val draw_tree : tree -> Croquis.Picture.t
val draw_branch : branch -> Croquis.Picture.t
