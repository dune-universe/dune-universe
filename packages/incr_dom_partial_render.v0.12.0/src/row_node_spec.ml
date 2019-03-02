open! Core_kernel
open! Import

module Cell = struct
  type t =
    { attrs : Vdom.Attr.t list
    ; node : Vdom.Node.t
    }
end

type t =
  { row_attrs : Vdom.Attr.t list
  ; cells : Cell.t list
  }
