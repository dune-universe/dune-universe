module Json_of_channel : module type of Json_of_jsonm_monad.Make(struct
    type 'a t = 'a

    let return _ = failwith "error"
    let (>>=) _ _ = failwith "error"
  end)

type json = Json_of_channel.json

(** [json_of_channel] - decode a text stream from [in_channel] to a [json] type *)
val json_of_channel : in_channel -> (json, string) result

(** [json_of_channel_exn] - the same as [json_of_channel] but raises on error *)
val json_of_channel_exn : in_channel -> json

(** [json_to_channel] - encode a [json] type to channel [out_channel] *)
val json_to_channel : out_channel -> json -> (unit, string) result

(** [json_to_channel_exn] - the same as [json_to_channel] but raises on error *)
val json_to_channel_exn : out_channel -> json -> unit
