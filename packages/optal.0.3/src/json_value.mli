type json =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of json list
  | `O of (string * json) list ]

type escape = ((int * int) * (int * int)) * Jsonm.error

exception Escape of escape

type result =
  [ `JSON of json
  | `Error of escape ]

type error =
  | Loading of escape
  | Not_an_object

exception Error of (error * string)

val value_of_json : json -> Value.value

val load_string : ?filename:string -> string -> Value.Env.t -> Value.Env.t

val load_file : in_channel * string -> Value.Env.t -> Value.Env.t

val report_error : Format.formatter -> string -> error -> unit
