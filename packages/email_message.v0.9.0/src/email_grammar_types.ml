(* Some simple, lightweight types for parser output *)
type header = (string * string) list;;

type content_offset = [`Content_offset of int | `Bad_headers of int | `Truncated];;
type message = [`Message of (header * content_offset)];;
