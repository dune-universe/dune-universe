exception Json_error of string

let json_error msg = raise (Json_error msg)

type lexer_state = {
  buf : Buffer.t;
  mutable lnum : int;
  mutable bol : int;
  mutable fname : string option;
}

let init_lexer ?buf:_ ?fname ?(lnum = 1) () =
  {
    buf = Buffer.create 16  (* unused *)
  ; lnum = lnum
  ; bol = 0
  ; fname = fname
  }

module Common_reader (Compliance : Compliance.S) = struct
  module Internal_reader = struct
    module Lexxer = Compliant_lexxer.Make(Compliance)
    module Parser = Parser.Make(Compliance)
    include Reader_string_file.Make (Lexxer) (Parser)
  end

  type json = Internal_reader.json
  type t = json
  type json_line = [ `Json of t | `Exn of exn ]

  (* Helper functions *)
  let error_to_string (error_info:Error_info.t) fname lnum =
    let lnum = match lnum with Some lnum -> lnum | None -> 1 in
    let info = { error_info with line = lnum + error_info.line - 1 } in
    let fname = match fname with
      | None -> "Line"
      | Some name -> "File " ^ name ^ ", line"
    in
    let loc = Printf.sprintf "%s %d chars %d-%d: " fname info.line info.start_char info.end_char in
    loc ^ info.msg

  let apply_and_handle_errors f a fname lnum =
    match f a with
    | Ok json -> json
    | Error error_info -> json_error (error_to_string error_info fname lnum)

  (* Readers *)
  let from_string ?buf:_ ?fname ?lnum s =
    apply_and_handle_errors Internal_reader.json_of_string_error_info s fname lnum

  let from_channel ?buf:_ ?fname ?lnum in_channel =
    apply_and_handle_errors Internal_reader.json_of_channel_error_info in_channel fname lnum

  let from_file ?buf:_ ?fname ?lnum filename =
    apply_and_handle_errors Internal_reader.json_of_file_error_info filename fname lnum

  let from_lexbuf lexstate ?stream lexbuf =
    let fname = lexstate.fname in
    let lnum = Some lexstate.lnum in
    match Internal_reader.json_of_lexbuf_error_info_compat ?stream lexbuf with
    | Ok (Some json) -> json
    | Ok None -> json_error "Blank input data"
    | Error error_info -> json_error (error_to_string error_info fname lnum)

  let read_t lexstate lexbuf = from_lexbuf lexstate lexbuf

  let stream_apply_and_handle_errors stream_f a fname lnum =
    let stream = stream_f a in
    let f _i =
      match Stream.next stream with
      | v -> Some v
      | exception Stream.Failure -> None
      | exception Error_info.Json_error_info err_info ->
        json_error (error_to_string err_info fname lnum)
    in
    Stream.from f

  let stream_from_string ?buf:_ ?fname ?lnum s =
    stream_apply_and_handle_errors Internal_reader.stream_from_string_error_info s fname lnum

  let stream_from_channel ?buf:_ ?(fin = fun () -> ()) ?fname ?lnum in_channel =
    stream_apply_and_handle_errors (Internal_reader.stream_from_channel_error_info ~fin) in_channel fname lnum

  let stream_from_file ?buf:_ ?fname ?lnum filename =
    stream_apply_and_handle_errors Internal_reader.stream_from_file_error_info filename fname lnum

  let stream_from_lexbuf lexstate ?(fin = fun () -> ()) lexbuf =
    let stream = Internal_reader.stream_from_lexbuf_error_info lexbuf in
    let f _i =
      match Stream.next stream with
      | v -> Some v
      | exception Stream.Failure -> fin (); None
      | exception Error_info.Json_error_info err_info ->
        fin (); json_error (error_to_string err_info lexstate.fname (Some lexstate.lnum))
    in
    Stream.from f


  let linestream_from_channel ?buf:_ ?(fin = fun () -> ()) ?fname ?(lnum = 1) ic =
    let f i =
      try
        let lnum = lnum + i in
        let line = input_line ic in Some (`Json (from_string ?fname ~lnum line))
      with
        | End_of_file -> fin (); None
        | exn_ -> fin (); Some (`Exn exn_)
    in
    Stream.from f

  let linestream_from_file ?buf:_ ?fname:_ ?lnum:_ filename =
    let ic = open_in filename in
    linestream_from_channel ~fin:(fun () -> close_in ic) ic
end

module Common_writer (Compliance : Compliance.S) = struct
  module Internal_writer = struct
    include Writer_string.Make(Compliance)
    include Writer_file.Make(Compliance)
    include Pretty.Make(Compliance)
  end

  let to_standard json =
    let rec map node =
      match node with
      | `Null -> `Null
      | `Bool _ as v -> v
      | `Int _ as v -> v  (* int is ok on output *)
      | `Intlit v -> `String v
      | `Float _ as v -> v
      | `Floatlit v -> `Float (float_of_string v)
      | `String _ as v -> v
      | `Stringlit s -> begin
        match String.length s with
        | 0 | 1 -> `String s         (* malformed, should have double-quotes at start and end *)
        | _ -> `String (String.sub s 1 (String.length s - 2))
        end
      | `List l -> `List (List.map map l)
      | `Assoc a -> `Assoc (List.map (fun (id, v) -> (id, map v)) a)
      | `Tuple tpl -> `List (List.map map tpl)
      | `Variant (name, jopt) -> begin
         match jopt with
         | None -> `String name
         | Some v -> `List [ `String name; (map v) ]
        end
    in
    map json

  (* Writers *)

  let to_string ?buf:_ ?len:_ ?(std = false) json =
    if std then Internal_writer.to_string (to_standard json) else Internal_writer.to_string json

  let to_channel ?buf:_ ?len:_ ?(std = false) out_channel json =
    if std then Internal_writer.to_channel out_channel (to_standard json)
    else Internal_writer.to_channel out_channel json

  let to_file ?len:_ ?(std = false) filename json =
    if std then Internal_writer.to_file filename (to_standard json)
    else Internal_writer.to_file filename json

  let to_outbuf ?(std = false) buf json =
    if std then Internal_writer.to_buffer buf (to_standard json) else Internal_writer.to_buffer buf json

  let to_output ?buf:_ ?len:_ ?std out json =
    let str = to_string ?std json in
    out#output str 0 (String.length str)

  let stream_to_string ?buf:_ ?len:_ ?std stream =
    let buf = Buffer.create 100 in
    let () = Stream.iter
      (fun json -> to_outbuf ?std buf json;  Buffer.add_char buf '\n') stream
    in
    Buffer.contents buf

  let stream_to_channel ?buf:_ ?len:_ ?std oc stream =
    Stream.iter (fun json -> to_channel ?std oc json; output_char oc '\n') stream

  let stream_to_file ?len:_ ?std filename stream =
    let oc = open_out filename in
    try (stream_to_channel ?std oc stream; close_out oc) with
    | exn -> close_out oc; raise exn

  let stream_to_outbuf ?std buf stream =
    Stream.iter (fun json -> to_outbuf ?std buf json; Buffer.add_char buf '\n') stream

  let write_t buf json = to_outbuf buf json

  (* Pretty printers *)
  let pretty_print ?(std = false) out json =
    if std then Internal_writer.pretty_print out (to_standard json)
    else Internal_writer.pretty_print out json

  let pretty_to_string ?(std = false) json =
    if std then Internal_writer.pretty_print_to_string (to_standard json)
    else Internal_writer.pretty_print_to_string json

  let pretty_to_channel ?(std = false) oc json =
    if std then Internal_writer.pretty_print_to_channel oc (to_standard json)
    else Internal_writer.pretty_print_to_channel oc json

  (* Utilities *)
  let show json = Utilities.json_to_string_repr json
  let pp out json = Format.pp_print_string out (show json)
end

module Basic = struct
  module Compliance = struct
    type json = Json.Basic.json
    type json_stream = Json_stream.Basic.json

    open Tokens

    let lex_string s = Lexxer_utils.unescape_string s
    let lex_number token = token
    let lex_integer token = token
    let lex_largeint _ = COMPLIANCE_ERROR "Integer out of bounds"

    let lex_variant _ = false
    let lex_tuple _ = false

    let comment_check () = Ok ()

    let number_to_string f =
      match classify_float f with
      | FP_normal | FP_subnormal | FP_zero ->
        Json_float.string_of_float_json f
      | FP_infinite ->
        if f < 0. then "-Infinity" else "Infinity"
      | FP_nan ->
        "NaN"

    let largeint s = `Float (float_of_string s)
    let integer i = `Int i
    let null = `Null
    let string s = `String s
    let bool b = `Bool b
    let assoc a = `Assoc a
    let list l = `List l
    let tuple _l = raise (Failure "tuples not supported in yojson basic mode")
    let variant _l = raise (Failure "variants not supported in yojson basic mode")

    let number = function
    | `Float f ->     `Float f
    | `Infinity ->    `Float (1.0 /. 0.0)
    | `Neginfinity -> `Float (-1.0 /. 0.0)
    | `Nan ->         `Float (0.0 /. 0.0)
    | `Floatlit _ ->  raise (Failure "floatlit not supported in yojson basic mode")

    module Stream = struct
      let number = number
      let largeint = largeint
      let integer = integer
      let null = null
      let string = string
      let bool = bool

      let array_start () = `As
      let array_end () = `Ae
      let object_start () = `Os
      let object_end () = `Oe
      let tuple_start () = raise (Failure "tuples not supported in yojson basic mode")
      let tuple_end () = raise (Failure "tuples not supported in yojson basic mode")
      let variant_start () = raise (Failure "variants not supported in yojson basic mode")
      let variant_end () = raise (Failure "variants not supported in yojson basic mode")
      let name s = `Name s
    end
  end

  include Common_reader(Compliance)
  include Common_writer(Compliance)

  let prettify ?std instr = from_string instr |> pretty_to_string ?std
  let compact ?std instr = from_string instr |> to_string ?std

  let equal = Utilities.equal
  let sort = Process.Basic.sort

  module Util = struct
    include Process.Basic
  end
end

module Safe = struct
  module Compliance = struct
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

    type json_stream = Json_stream.Extended.json  (* yojson interface does not support streaming *)

    let lex_string s = Lexxer_utils.unescape_string s
    let lex_number token = token
    let lex_integer token = token
    let lex_largeint token = token

    let lex_variant _ = true
    let lex_tuple _ = true

    let comment_check () = Ok ()

    let number_to_string f =
      match classify_float f with
      | FP_normal | FP_subnormal | FP_zero ->
        Json_float.string_of_float_json f
      | FP_infinite ->
        if f < 0. then "-Infinity" else "Infinity"
      | FP_nan ->
        "NaN"

    let largeint s = `Intlit s
    let integer i = `Int i
    let null = `Null
    let string s = `String s
    let bool b = `Bool b
    let assoc a = `Assoc a
    let list l = `List l
    let tuple l = `Tuple l
    let variant k v = `Variant (k, v)

    let number = function
    | `Float f ->     `Float f
    | `Infinity ->    `Float (1.0 /. 0.0)
    | `Neginfinity -> `Float (-1.0 /. 0.0)
    | `Nan ->         `Float (0.0 /. 0.0)
    | `Floatlit _ ->  raise (Failure "floatlit not supported in yojson safe mode")

    module Stream = struct
      let number = number
      let largeint = largeint
      let integer = integer
      let null = null
      let string = string
      let bool = bool

      let array_start () = `As
      let array_end () = `Ae
      let object_start () = `Os
      let object_end () = `Oe
      let tuple_start () = `Ts
      let tuple_end () = `Te
      let variant_start () = `Vs
      let variant_end () = `Ve
      let name s = `Name s
    end
  end

  include Common_reader(Compliance)
  include Common_writer(Compliance)

  let prettify ?std instr = from_string instr |> pretty_to_string ?std
  let compact ?std instr = from_string instr |> to_string ?std

  let equal = Utilities.equal
  let sort = Process.Extended.sort

  let to_basic json : Basic.json =
    let rec map node =
      match node with
      | `Null -> `Null
      | `Bool _ as v -> v
      | `Int _ as v -> v
      | `Intlit v -> `String v
      | `Float _ as v -> v
      | `String _ as v -> v
      | `List l -> `List (List.map map l)
      | `Assoc a -> `Assoc (List.map (fun (id, v) -> (id, map v)) a)
      | `Tuple tpl -> `List (List.map map tpl)
      | `Variant (name, jopt) ->
         match jopt with
         | None -> `String name
         | Some v -> `List [ `String name; (map v) ]
    in
    map json

  module Util = struct
    include Process.Yojson_safe
  end
end

module Raw = struct
  module Compliance = struct

  type json =
    [
    | `Null
    | `Bool of bool
    | `Intlit of string
    | `Floatlit of string
    | `Stringlit of string
    | `Assoc of (string * json) list
    | `List of json list
    | `Tuple of json list
    | `Variant of (string * json option)
    ]

    type json_stream = Json_stream.Extended.json (* yojson interface does not support streaming *)

    let lex_string s = "\"" ^ s ^ "\""
    let lex_number token = token
    let lex_integer token = token
    let lex_largeint token = token

    let lex_variant _ = true
    let lex_tuple _ = true

    let comment_check () = Ok ()

    let number_to_string f =
      match classify_float f with
      | FP_normal | FP_subnormal | FP_zero ->
        Json_float.string_of_float_json f
      | FP_infinite ->
        if f < 0. then "-Infinity" else "Infinity"
      | FP_nan ->
        "NaN"

    let largeint s = `Intlit s
    let integer i = `Intlit (string_of_int i)
    let null = `Null
    let string s = `Stringlit s
    let bool b = `Bool b
    let assoc a = `Assoc a
    let list l = `List l
    let tuple l = `Tuple l
    let variant k v = `Variant (k, v)

    let number = function
    | `Float f ->     `Floatlit (string_of_float f)
    | `Infinity ->    `Floatlit "Infinity"
    | `Neginfinity -> `Floatlit "-Infinity"
    | `Nan ->         `Floatlit "NaN"
    | `Floatlit f ->  `Floatlit f

    module Stream = struct
      let number = number
      let largeint = largeint
      let integer = integer
      let null = null
      let string = string
      let bool = bool

      let array_start () = `As
      let array_end () = `Ae
      let object_start () = `Os
      let object_end () = `Oe
      let tuple_start () = `Ts
      let tuple_end () = `Te
      let variant_start () = `Vs
      let variant_end () = `Ve
      let name s = `Name s
    end
  end

  include Common_reader(Compliance)
  include Common_writer(Compliance)

  let prettify ?std instr = from_string instr |> pretty_to_string ?std
  let compact ?std instr = from_string instr |> to_string ?std

  let equal = Utilities.equal
  let sort = Process.Extended.sort
end
