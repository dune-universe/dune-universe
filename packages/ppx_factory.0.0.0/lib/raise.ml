open Ppxlib

let errorf ~loc fmt =
  Printf.ksprintf (Location.raise_errorf ~loc "ppx_factory: %s") fmt

module Default = struct
  let errorf ~loc fmt =
    Printf.ksprintf (Location.raise_errorf ~loc "ppx_factory.default: %s") fmt
end

module Factory = struct
  let errorf ~loc fmt =
    Printf.ksprintf (Location.raise_errorf ~loc "ppx_factory.factory: %s") fmt

  let unhandled_type_kind ~loc kind =
    errorf ~loc "cannot derive from %s type. Has to be a record or variant type." kind
end
