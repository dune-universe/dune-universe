module Chunk : sig
  type t =
    { ocaml_code        : string
    ; toplevel_response : string
    }
  [@@deriving sexp]
end

module Part : sig
  type t =
    { name   : string
    ; chunks : Chunk.t list
    }
  [@@deriving sexp]
end

module Document : sig
  type t =
    { parts   : Part.t list
    ; matched : bool (** Whether the actual output matched the expectations *)
    }
  [@@deriving sexp]
end
