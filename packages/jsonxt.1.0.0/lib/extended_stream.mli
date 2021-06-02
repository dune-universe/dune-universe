(** [Extended_stream] supports parsing and writing JSON data that
    conforms to the {!type:Json_stream.Extended.json} type as a stream
    of {!type:Json_stream.Extended.json} objects.  This supports non-standard
    JSON types including integer as well as tuples and variants introduced
    by [Yojson] *)

type json_stream = Json_stream.Extended.json

(** {1 Reader functions} *)

include (Reader_stream.Reader_stream with type json_stream := json_stream)

(** {1 Writer functions} *)

include (Writer_stream_intf.Intf with type json_stream := json_stream)
