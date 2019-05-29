open Ppxlib

let errorf ~loc fmt =
  Printf.ksprintf (Location.raise_errorf ~loc "ppx_enum: %s") fmt

module Enum = struct
  let errorf ~loc fmt =
    Printf.ksprintf (Location.raise_errorf ~loc "ppx_enum.enum: %s") fmt

  let unhandled_type_kind ~loc kind =
    errorf ~loc "cannot derive from %s type. Enums can only be derived from variants without arguments." kind
end
