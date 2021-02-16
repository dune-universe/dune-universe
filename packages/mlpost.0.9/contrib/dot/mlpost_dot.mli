open Mlpost
(** Place figures, boxes or boxlikes with graphviz *)

module Dot : sig
  module Make (B : Signature.Boxlike) : sig
    type node

    type edge = node * node

    val mknode : B.t -> node
    (** creates an abstract node from a boxlike *)

    val place :
      ?orient:[ `TB | `LR | `BT | `RL ] ->
      node list ->
      edge list ->
      B.t list * Path.t list
    (** [place ~orient nodes edges] returns a concrete
          representation of the abstract directed graph composed by
          [nodes] linked by [edges]. The concrete representation is
          composed by the list of all the boxlikes of [nodes] placed
          by dot and by the list of paths representing the [edges]
          drawn by dot

          @param orient specifies the orientation of the graph :
          - `TB top to bottom (default)
          - `LR left to right
          - `BT bottom to top
          - `RL right to left

      *)
  end
end
