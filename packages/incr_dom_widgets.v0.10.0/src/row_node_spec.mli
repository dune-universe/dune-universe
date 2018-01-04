open! Core_kernel
open! Import

module Attrs : sig
  type t =
    { style : (string * string) list
    (* [attrs] should not contain a [style] attribute, or it may be overwritten *)
    ; other_attrs : Vdom.Attr.t list
    }

  val create
    :  ?style : (string * string) list
    -> ?attrs : Vdom.Attr.t list
    -> unit
    -> t

  val combine : t list -> t

  val to_vdom_attrs : t -> Vdom.Attr.t list
end

module Cell : sig
  (* Each cell node is wrapped in a <td> node with the given [attrs] and possibly
     some additional attributes *)
  type t =
    (* [attrs] should not include an [id] attribute, it may be overwritten *)
    { attrs : Attrs.t
    ; node : Vdom.Node.t
    }
end

(* The cells are wrapped in a <tr> node with the given [row_attrs] and possibly some
   additional attributes *)
type t =
  (* [row_attrs] should not contain an [id] attribute, or it will be overwritten *)
  { row_attrs : Attrs.t
  ; cells : Cell.t list
  }
