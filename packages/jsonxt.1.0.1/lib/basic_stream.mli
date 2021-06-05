(** [Basic_stream] supports parsing and writing JSON data that
    conforms to the {!type:Json_stream.Basic.json} type as a stream
    of {!type:Json_stream.Basic.json} objects.  This includes support
    for integers which are not part of the JSON standard *)

type json_stream = Json_stream.Basic.json

(** {1 Reader functions} *)

include (Reader_stream.Reader_stream with type json_stream := json_stream)

(** {1 Writer functions} *)

include (Writer_stream_intf.Intf with type json_stream := json_stream)
