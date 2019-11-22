open! Core_kernel
open! Import

module Cell : sig
  (* Each cell node is wrapped in a <td> node with the given [attrs] and possibly
     some additional attributes *)

  type t =
    { (* [attrs] should not include an [id] attribute, it may be overwritten *)
      attrs : Vdom.Attr.t list
    ; node : Vdom.Node.t
    }
end

(* The cells are wrapped in a <tr> node with the given [row_attrs] and possibly some
   additional attributes *)

type t =
  { (* [row_attrs] should not contain an [id] attribute, or it will be overwritten *)
    row_attrs : Vdom.Attr.t list
  ; cells : Cell.t list
  }
