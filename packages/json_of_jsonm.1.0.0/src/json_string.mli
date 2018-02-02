module Json_of_string : module type of Json_of_jsonm_monad.Make(struct
    type 'a t = 'a

    let return _ = failwith "error"
    let (>>=) _ _ = failwith "error"
  end)

type json = Json_of_string.json

(** [json_of_string] - decode a [string] to a [json] type *)
val json_of_string : string -> (json, string) result

(** [json_of_string_exn] - the same as [json_of_string] but raises on error *)
val json_of_string_exn : string -> json

(** [json_to_string] - encode a [json] type to a [string] *)
val json_to_string : json -> (string, string) result

(** [json_to_string_exn] - the same as [json_to_string] but raises on error *)
val json_to_string_exn : json -> string

(** [json_to_string_hum] - same as [json_to_string_exn] but formats the output for
    humans to read *)
val json_to_string_hum : json -> (string, string) result

type t = Json_of_string.json

(** [of_string] - decode a [string] to a [json] type, raises on error *)
val of_string : string -> t

(** [to_string] - encode a [json] type to a  [string], raises on error *)
val to_string : t -> string
