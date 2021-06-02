module type Reader_string_file = sig
  type json

  val json_of_string : string -> (json, string) result
  val json_of_string_exn : string -> json
  val json_of_file : string -> (json, string) result
  val json_of_file_exn : string -> json
  val json_of_channel : in_channel -> (json, string) result
  val json_of_channel_exn : in_channel -> json
  val json_of_function : (bytes -> int -> int) -> (json, string) result
  val json_of_function_exn : (bytes -> int -> int) -> json
  val json_of_lexbuf : Lexing.lexbuf -> (json, string) result
  val json_of_lexbuf_exn : Lexing.lexbuf -> json
  val of_string : string -> json
  val of_file : string -> json
  val of_channel : in_channel -> json
  val of_function : (bytes -> int -> int) -> json

  val json_of_string_error_info : string -> (json, Error_info.t) result
  val json_of_file_error_info : string -> (json, Error_info.t) result
  val json_of_channel_error_info : in_channel -> (json, Error_info.t) result
  val json_of_function_error_info : (bytes -> int -> int) -> (json, Error_info.t) result
  val json_of_lexbuf_error_info : Lexing.lexbuf -> (json, Error_info.t) result
  val json_of_lexbuf_error_info_compat : ?stream:bool -> Lexing.lexbuf -> (json option, Error_info.t) result

  val stream_from_string : string -> json Stream.t
  val stream_from_channel : ?fin:(unit -> unit) -> in_channel -> json Stream.t
  val stream_from_file : string -> json Stream.t
  val stream_from_function : (bytes -> int -> int) -> json Stream.t
  val stream_from_lexbuf : Lexing.lexbuf -> json Stream.t

  val stream_from_string_error_info : string -> json Stream.t
  val stream_from_channel_error_info : ?fin:(unit -> unit) -> in_channel -> json Stream.t
  val stream_from_file_error_info : string -> json Stream.t
  val stream_from_function_error_info : (bytes -> int -> int) -> json Stream.t
  val stream_from_lexbuf_error_info : Lexing.lexbuf -> json Stream.t
end

module Make (Lexxer : Compliant_lexxer.Lex ) (Parser : Parser.Parser) : Reader_string_file
  with type json = Parser.Compliance.json
= struct
  type json = Parser.Compliance.json

  let read_json' ~lexbuf =
    let reader () = Lexxer.read lexbuf in
    match Parser.decode ~reader with
    | Ok None -> Error "empty input"
    | Ok (Some res) -> begin
      match reader () with
      | EOF -> Ok res
      | exception Lexxer_utils.Lex_error err -> Error err
      | tok -> Error ("junk after end of JSON value: " ^ (Token_utils.token_to_string tok))
      end
    | Error s -> Error s

  let read_json ~lexbuf =
    match read_json' ~lexbuf with
    | Ok _ as res -> res
    | Error s ->
      let err_info = Error_info.create_from_lexbuf lexbuf s in
      Error (Error_info.to_string err_info)

  let json_of_string s =
    let lexbuf = Lexing.from_string s in
    read_json ~lexbuf

  let json_of_string_exn s =
    match json_of_string s with
    | Ok res -> res
    | Error s -> raise (Failure s)

  let of_string s = json_of_string_exn s

  let json_of_file filename =
    try begin
      let inc = open_in filename in
      let lexbuf = Lexing.from_channel inc in
      let res = read_json ~lexbuf in
        close_in inc;
        res
    end
    with Sys_error err -> Error err

  let json_of_file_exn filename =
    match json_of_file filename with
    | Ok res -> res
    | Error s -> raise (Failure s)

  let json_of_channel inc =
    let lexbuf = Lexing.from_channel inc in
    read_json ~lexbuf

  let json_of_channel_exn inc =
    match json_of_channel inc with
    | Ok res -> res
    | Error s -> raise (Failure s)

  let json_of_function f =
    let lexbuf = Lexing.from_function f in
    read_json ~lexbuf

  let json_of_lexbuf lexbuf =
    read_json ~lexbuf

  let json_of_lexbuf_exn lexbuf =
    match json_of_lexbuf lexbuf with
    | Ok res -> res
    | Error s -> raise (Failure s)

  let json_of_function_exn f =
    match json_of_function f with
    | Ok res -> res
    | Error s -> raise (Failure s)

  let of_file = json_of_file_exn
  let of_channel = json_of_channel_exn
  let of_function = json_of_function_exn

  (* Error_info.t returning functions *)

  let read_json_error_info ~lexbuf =
    match read_json' ~lexbuf with
    | Ok _ as res -> res
    | Error err ->
      let err_info = Error_info.create_from_lexbuf lexbuf err in
      Error err_info

  let json_of_string_error_info s =
    let lexbuf = Lexing.from_string s in
    read_json_error_info ~lexbuf

  let json_of_channel_error_info inc =
    let lexbuf = Lexing.from_channel inc in
    read_json_error_info ~lexbuf

  let json_of_file_error_info filename =
    try begin
      let inc = open_in filename in
      let res = json_of_channel_error_info inc in
        close_in inc;
        res
    end
    with Sys_error err -> Error { Error_info.line = 0; start_char = 0; end_char = 0; msg = err }

  let json_of_function_error_info f =
    let lexbuf = Lexing.from_function f in
    read_json_error_info ~lexbuf

  let json_of_lexbuf_error_info lexbuf =
    read_json_error_info ~lexbuf

  (* Internal compatibility function supporting the stream flag *)

  let json_of_lexbuf_error_info_compat ?(stream = false) lexbuf =
    let reader () = Lexxer.read lexbuf in
    let res = match Parser.decode ~reader with
    | Ok None -> if stream then Ok None else Error "empty input"
    | Ok (Some res) -> begin
      match stream with
      | true -> Ok (Some res)
      | false -> begin
        match reader () with
        | EOF -> Ok (Some res)
        | exception Lexxer_utils.Lex_error err -> Error err
        | tok -> Error ("junk after end of JSON value: " ^ (Token_utils.token_to_string tok))
        end
      end
    | Error s -> Error s
    in
    match res with
    | Ok res -> Ok res
    | Error s ->
      let err_info = Error_info.create_from_lexbuf lexbuf s in
      Error err_info


  (* Stream.t returning functions *)

  let read_json_stream ~fin ~lexbuf =
    let reader () = Lexxer.read lexbuf in
    let f _i =
      match Parser.decode ~reader with
      | Ok None -> fin (); None
      | Ok (Some res) -> Some res
      | Error err ->
        let () = fin () in
        let err_info = Error_info.create_from_lexbuf lexbuf err in
        let msg = Error_info.to_string err_info in
        raise (Failure msg)
    in
    Stream.from f

  let stream_from_string s =
    let lexbuf = Lexing.from_string s in
    read_json_stream ~fin:(fun () -> ()) ~lexbuf

  let stream_from_channel ?(fin = fun () -> ()) inc =
    let lexbuf = Lexing.from_channel inc in
    read_json_stream ~fin ~lexbuf

  let stream_from_function f =
    let lexbuf = Lexing.from_function f in
    read_json_stream ~fin:(fun () -> ()) ~lexbuf

  let stream_from_file filename =
    let inc = open_in filename in
    stream_from_channel ~fin:(fun () -> close_in inc) inc

  let stream_from_lexbuf lexbuf =
    read_json_stream ~fin:(fun () -> ()) ~lexbuf

  (* Stream.t Json_error_info raising functions *)

  let read_json_stream_error_info ~fin ~lexbuf =
    let reader () = Lexxer.read lexbuf in
    let f _i =
      match Parser.decode ~reader with
      | Ok None -> fin (); None
      | Ok (Some res) -> Some res
      | Error err ->
        let () = fin () in
        let err_info = Error_info.create_from_lexbuf lexbuf err in
        raise (Error_info.Json_error_info err_info)
    in
    Stream.from f

  let stream_from_string_error_info s =
    let lexbuf = Lexing.from_string s in
    read_json_stream_error_info ~fin:(fun () -> ()) ~lexbuf

  let stream_from_channel_error_info ?(fin = fun () -> ()) inc =
    let lexbuf = Lexing.from_channel inc in
    read_json_stream_error_info ~fin ~lexbuf

  let stream_from_function_error_info f =
    let lexbuf = Lexing.from_function f in
    read_json_stream_error_info ~fin:(fun () -> ()) ~lexbuf

  let stream_from_file_error_info filename =
    let inc = open_in filename in
    stream_from_channel_error_info ~fin:(fun () -> close_in inc) inc

  let stream_from_lexbuf_error_info lexbuf =
    read_json_stream_error_info ~fin:(fun () -> ()) ~lexbuf

end
