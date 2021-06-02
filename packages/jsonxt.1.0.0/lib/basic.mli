(** [Basic] supports parsing and writing JSON data that conforms to the
    {!type:Json.Basic.json} json type.  This includes support for
    integers which are not part of the JSON standard
    
    The maximum/minimum size of an integer is architecture specific,
    typically 30 or 62 bits depending on the platform.  In cases where
    the integer overflows the value is converted to a `Float.  For
    integers in the range (+/-)2^53 there is no loss of precision *)

type json = Json.Basic.json
type t = json

(** {1 Reader functions} *)

include (Reader_string_file.Reader_string_file with type json := json)

(** {1 Writer functions} *)

include (Writer_intf.Intf with type json := Json.Basic.json)

(** {1 Processing functions} *)

module Process : sig
  include (module type of Process.Basic)
end

(** {1 Internal modules} *)

module Compliance : Compliance.S with
  type json = Json.Basic.json
  and type json_stream = Json_stream.Basic.json
