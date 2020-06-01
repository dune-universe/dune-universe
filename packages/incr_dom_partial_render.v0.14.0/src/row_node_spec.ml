open! Core_kernel
open! Import

module Cell = struct
  type t =
    { attrs : Vdom.Attr.t list
    ; nodes : Vdom.Node.t list
    }
end

type t =
  { row_attrs : Vdom.Attr.t list
  ; cells : Cell.t list
  }
