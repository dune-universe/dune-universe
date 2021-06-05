module type Reader_stream = sig
  type json_stream
  type stream

  val json_stream_of_string : string -> stream
  val json_stream_of_channel : in_channel -> stream
  val json_stream_of_function : (bytes -> int -> int) -> stream
  val decode_stream : stream -> (json_stream option, string) result
  val stream_from_string : string -> json_stream Stream.t
  val stream_from_channel : in_channel -> json_stream Stream.t
  val stream_from_function : (bytes -> int -> int) -> json_stream Stream.t
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser_stream.Parser) : Reader_stream
  with type json_stream = Parser.Compliance.json_stream
= struct
  type json_stream = Parser.Compliance.json_stream
  type stream = {
    parser_t : Parser.t
  ; lexbuf : Lexing.lexbuf
  }

  let create_parser ~lexbuf =
    let reader () = Lexxer.read lexbuf in
      { parser_t = Parser.create ~reader; lexbuf = lexbuf }

  let json_stream_of_string s =
    let lexbuf = Lexing.from_string s in
      create_parser ~lexbuf

  let json_stream_of_channel inc =
    let lexbuf = Lexing.from_channel inc in
      create_parser ~lexbuf

  let json_stream_of_function f =
    let lexbuf = Lexing.from_function f in
      create_parser ~lexbuf

  let decode_stream t =
    match Parser.decode t.parser_t with
    | Error err ->
      let err_info = Error_info.create_from_lexbuf t.lexbuf err in
      Error (Error_info.to_string err_info)
    | v -> v

  let stream_from_lexbuf lexbuf =
    let decoder = create_parser ~lexbuf in
    let f _i =
      match decode_stream decoder with
      | Ok None -> None
      | Ok Some v -> Some v
      | Error err -> raise (Failure err)
    in
    Stream.from f

  let stream_from_string s =
    let lexbuf = Lexing.from_string s in
    stream_from_lexbuf lexbuf

  let stream_from_channel inc =
    let lexbuf = Lexing.from_channel inc in
    stream_from_lexbuf lexbuf

  let stream_from_function f =
    let lexbuf = Lexing.from_function f in
    stream_from_lexbuf lexbuf

end
