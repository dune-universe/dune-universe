open! Core_kernel
open! Import

module Attrs = struct
  type t =
    { style : (string * string) list
    ; other_attrs : Vdom.Attr.t list
    }

  let create ?(style=[]) ?(attrs=[]) () = { style; other_attrs = attrs }

  let combine t_list =
    { style = List.concat_map t_list ~f:(fun t -> t.style)
    ; other_attrs = List.concat_map t_list ~f:(fun t -> t.other_attrs)
    }

  let to_vdom_attrs t = Vdom.Attr.style t.style :: t.other_attrs
end

module Cell = struct
  type t =
    { attrs : Attrs.t
    ; node : Vdom.Node.t
    }
end

type t =
  { row_attrs : Attrs.t
  ; cells : Cell.t list
  }
