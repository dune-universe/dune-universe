open Mlpost

module Make (B : Signature.Boxlike) : sig
  type node

  type edge = node * node

  val mknode : B.t -> node

  val mkedge : node -> node -> edge

  val mkedges : (node * node) list -> edge list

  val place :
    ?orient:[ `TB | `LR | `BT | `RL ] ->
    node list ->
    edge list ->
    B.t list * Path.t list
end
