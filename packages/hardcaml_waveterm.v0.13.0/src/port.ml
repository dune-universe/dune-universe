open! Import

module Type = struct
  type t =
    | Input
    | Output
    | Internal
  [@@deriving compare, sexp_of]
end

type t =
  { type_ : Type.t
  ; port_name : Port_name.t
  ; width : int
  }
[@@deriving compare, sexp_of]
