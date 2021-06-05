(** [Process] provides functions for processing Jsonxt json trees.
    This includes functions to map, flatten and extract data *)

(** [Strict] supports processing JSON data that conforms to the
    {!type:Json.Strict.json} json type.  *)
module Strict : sig
  include (Process_intf.Shared with type json := Json.Strict.json)
end

(** [Basic] supports processing JSON data that conforms to the
    {!type:Json.Basic.json} json type.  *)
module Basic : sig
  include (Process_intf.Shared with type json := Json.Basic.json)
  include (Process_intf.Basic with type json := Json.Basic.json)
end

(** [Extended] supports processing JSON data that conforms to the
    {!type:Json.Extended.json} json type.  *)
module Extended : sig
  include (Process_intf.Shared with type json := Json.Extended.json)
  include (Process_intf.Basic with type json := Json.Extended.json)
  include (Process_intf.Extended with type json := Json.Extended.json)
end

(** [Yojson_safe] supports processing JSON data that conforms to
    Yojson's Safe json type. *)
module Yojson_safe : sig
  type json =
    [
    | `Null
    | `Bool of bool
    | `Int of int
    | `Intlit of string
    | `Float of float
    | `String of string
    | `Assoc of (string * json) list
    | `List of json list
    | `Tuple of json list
    | `Variant of (string * json option)
    ]
  include (Process_intf.Shared with type json := json)
  include (Process_intf.Basic with type json := json)
  include (Process_intf.Extended with type json := json)
end
