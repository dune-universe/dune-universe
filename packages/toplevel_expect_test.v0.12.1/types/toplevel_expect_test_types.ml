open Core_kernel

module Chunk = struct
  type t =
    { ocaml_code        : string
    ; toplevel_response : string
    }
  [@@deriving sexp]
end

module Part = struct
  type t =
    { name   : string
    ; chunks : Chunk.t list
    }
  [@@deriving sexp]
end

module Document = struct
  type t =
    { parts   : Part.t list
    ; matched : bool
    }
  [@@deriving sexp]
end
