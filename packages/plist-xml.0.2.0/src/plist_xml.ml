type t =
  [ `Bool of bool
  | `Data of string
  | `Date of float * float option
  | `Float of float
  | `Int of int
  | `String of string
  | `Array of t list
  | `Dict of (string * t) list ]

(* [stream] is a hard-coded coroutine. *)
let rec stream plist k =
  let start name = `Start_element(("", name), []) in
  (* yield value *)
  let rec (let^) value k =
    state := k;
    Some value
  (* repeatedly call state' until it finishes, then continue *)
  and (let^^) state' k =
    match !state' () with
    | Some v ->
       let^ () = v in
       (let^^) state' k
    | None -> k ()
  and state =
    ref (fun () ->
        match plist with
        | `Bool true ->
           let^ () = start "true" in
           let^ () = `End_element in
           k state
        | `Bool false ->
           let^ () = start "false" in
           let^ () = `End_element in
           k state
        | `Data str ->
           let^ () = start "data" in
           let^ () = `Text [Base64.encode_string str] in
           let^ () = `End_element in
           k state
        | `Date(timestamp, None) ->
           let^ () = start "date" in
           let^ () = `Text [ISO8601.Permissive.string_of_datetime timestamp] in
           let^ () = `End_element in
           k state
        | `Date(timestamp, Some tz) ->
           let^ () = start "date" in
           let^ () =
             `Text
               [ if tz = 0. then
                   (* Apple's spec only requires that plists support dates in
                      the format YYYY '-' MM '-' DD 'T' HH ':' MM ':' SS 'Z'

                      If a Z is parsed from the original input date, the
                      timezone float should be exactly 0. AFAIK it is correct
                      to test for float equality in this case. *)
                   ISO8601.Permissive.string_of_datetime timestamp ^ "Z"
                 else
                   ISO8601.Permissive.string_of_datetimezone (timestamp, tz) ]
           in
           let^ () = `End_element in
           k state
        | `Float f ->
           let^ () = start "real" in
           let^ () = `Text [Float.to_string f] in
           let^ () = `End_element in
           k state
        | `Int i ->
           let^ () = start "integer" in
           let^ () = `Text [Int.to_string i] in
           let^ () = `End_element in
           k state
        | `Array xs ->
           let^ () = start "array" in
           let rec loop = function
             | [] ->
                let^ () = `End_element in
                k state
             | x :: xs ->
                let^^ () = stream x (fun _ -> None) in
                loop xs
           in loop xs
        | `Dict dict ->
           let^ () = start "dict" in
           let rec loop = function
             | [] ->
                let^ () = `End_element in
                k state
             | (k, v) :: kvs ->
                let^ () = start "key" in
                let^ () = `Text [k] in
                let^ () = `End_element in
                let^^ () = stream v (fun _ -> None) in
                loop kvs
           in loop dict
        | `String str ->
           let^ () = start "string" in
           let^ () = `Text [str] in
           let^ () = `End_element in
           k state)
  in state

let cons v state =
  let k = !state in
  state := (fun () -> state := k; Some v);
  state

let signals ?encoding plist =
  let doctype =
    { Markup.doctype_name = Some "plist"
    ; public_identifier = Some "-//Apple//DTD PLIST 1.0//EN"
    ; system_identifier = None
    ; raw_text =
        Some "plist PUBLIC \
              \"-//Apple//DTD PLIST 1.0//EN\" \
              \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\""
    ; force_quirks = false }
  in
  let state =
    cons (`Xml Markup.{ version = "1.0"; encoding; standalone = None })
      (cons (`Doctype doctype)
         (cons (`Start_element(("", "plist"), [("", "version"), "1.0"]))
            (stream plist
               (fun state ->
                 state := (fun () -> None);
                 Some `End_element))))
  in
  Markup.stream (fun () -> !state ())

exception Parse_error of string

let end_of_doc () = raise (Parse_error "End of document")

let expected_closing () = raise (Parse_error "Expected closing tag")

module type IO = sig
  type s
  type _ io
  val next : ('a, s) Markup.stream -> 'a option io
  val peek : ('a, s) Markup.stream -> 'a option io
  val parse_xml :
    ?report:(Markup.location -> Markup.Error.t -> unit io) ->
    ?encoding:Markup.Encoding.t ->
    ?namespace:(string -> string option) ->
    ?entity:(string -> string option) ->
    ?context:[< `Document | `Fragment ] ->
    (char, s) Markup.stream -> s Markup.parser
  val bind : 'a io -> ('a -> 'b io) -> 'b io
  val return : 'a -> 'a io
end

module IO : IO with type s = Markup.sync and type 'a io = 'a = struct
  type s = Markup.sync
  type 'a io = 'a
  let next = Markup.next
  let peek = Markup.peek
  let parse_xml ?report ?encoding ?namespace ?entity ?context =
    Markup.parse_xml ?report ?encoding ?namespace ?entity ?context
  let bind x f = f x
  let return x = x
end

module type S = sig
  type s
  type _ io

  val plist_of_stream_exn :
    (Markup.content_signal, s) Markup.stream -> t io

  val parse_exn :
    ?report:(Markup.location -> Markup.Error.t -> unit io) ->
    ?encoding:Markup.Encoding.t ->
    ?namespace:(string -> string option) ->
    ?entity:(string -> string option) ->
    ?context:[< `Document | `Fragment ] ->
    (char, s) Markup.stream -> t io
end

let isn't_whitespace ch =
  ch <> ' ' && ch <> '\t' && ch <> '\r' && ch <> '\n'

let check_whitespace =
  String.iter (fun ch ->
      if isn't_whitespace ch then
        raise (Parse_error ("Unexpected character " ^ (String.make 1 ch)))
    )

let decode_base64 strs =
  let module M = Base64_rfc2045 in
  let decoder = M.decoder `Manual in
  let outbuf = Buffer.create 10 in
  let rec loop strs = function
    | `Await ->
       begin match strs with
       | str :: strs ->
          let buf = Buffer.create (String.length str) in
          String.iter (fun ch ->
              if isn't_whitespace ch then
                Buffer.add_char buf ch
            ) str;
          M.src decoder (Buffer.to_bytes buf) 0 (Buffer.length buf);
          loop strs (M.decode decoder)
       | [] ->
          M.src decoder (Bytes.create 0) 0 0;
          loop [] (M.decode decoder)
       end
    | `End -> Buffer.contents outbuf
    | `Flush str ->
       Buffer.add_string outbuf str;
       loop strs (M.decode decoder)
    | `Malformed str -> raise (Parse_error ("Malformed base64: " ^ str))
    | `Wrong_padding -> raise (Parse_error "Base64 wrong padding")
  in loop strs (M.decode decoder)

module Make (IO : IO) = struct
  type s = IO.s
  type 'a io = 'a IO.io

  let ( let* ) = IO.bind

  type start_or_end =
    [ `End_element
    | `Start_element of Markup.name * (Markup.name * string) list ]

  let rec skip_whitespace stream =
    let* next = IO.next stream in
    match next with
    | Some (`Text strs) ->
       List.iter check_whitespace strs;
       skip_whitespace stream
    | Some #start_or_end as some -> IO.return some
    | None -> IO.return None

  let rec peek_skip_whitespace stream =
    let* peeked = IO.peek stream in
    match peeked with
    | Some (`Text strs) ->
       List.iter check_whitespace strs;
       ignore (IO.next stream);
       peek_skip_whitespace stream
    | Some #start_or_end as some -> IO.return some
    | None -> IO.return None

  let rec parse_array acc stream =
    let* peeked = peek_skip_whitespace stream in
    match peeked with
    | Some `End_element ->
       (* Munch end element *)
       let* _ = skip_whitespace stream in
       IO.return (List.rev acc)
    | _ ->
       let* v = parse_val stream in
       parse_array (v :: acc) stream

  and parse_dict acc stream =
    let* next = skip_whitespace stream in
    match next with
    | Some `End_element -> IO.return (List.rev acc)
    | Some (`Start_element((_, "key"), _)) ->
       let* next = IO.next stream in
       begin match next with
       | Some (`Text [key]) ->
          let* next = IO.next stream in
          begin match next with
          | Some `End_element ->
             let* value = parse_val stream in
             parse_dict ((key, value) :: acc) stream
          | _ -> expected_closing ()
          end
       | Some (`Text (_ :: _)) ->
          raise (Parse_error "Key exceeds max string length")
       (* Empty key *)
       | Some `End_element ->
          let* value = parse_val stream in
          parse_dict (("", value) :: acc) stream
       | Some _ -> raise (Parse_error "Expected text inside key")
       | None -> end_of_doc ()
       end
    | Some (`Start_element((_, s), _)) ->
       raise (Parse_error ("Expected key, got " ^ s))
    | None -> end_of_doc ()

  and parse_val stream =
    let* next = skip_whitespace stream in
    match next with
    | Some (`Start_element((_, "array"), _)) ->
       let* arr = parse_array [] stream in
       IO.return (`Array arr)
    | Some (`Start_element((_, "data"), _)) ->
       let* next = IO.next stream in
       begin match next with
       | Some (`Text strs) -> IO.return (`Data (decode_base64 strs))
       | _ -> raise (Parse_error "Expected base64-encoded data")
       end
    | Some (`Start_element((_, "date"), _)) ->
       let* next = IO.next stream in
       begin match next with
       | Some (`Text [str]) ->
          let* next = IO.next stream in
          begin match next with
          | Some `End_element ->
             let time, offset =
               try ISO8601.Permissive.datetime_tz ~reqtime:false str with
               | Failure msg -> raise (Parse_error msg)
             in IO.return (`Date(time, offset))
          | _ -> expected_closing ()
          end
       | _ -> raise (Parse_error "Expected date")
       end
    | Some (`Start_element((_, "dict"), _)) ->
       let* dict = parse_dict [] stream in
       IO.return (`Dict dict)
    | Some (`Start_element((_, "false"), _)) ->
       let* next = IO.next stream in
       begin match next with
       | Some `End_element -> IO.return (`Bool false)
       | _ -> expected_closing ()
       end
    | Some (`Start_element((_, "integer"), _)) ->
       let* next = IO.next stream in
       begin match next with
       | Some (`Text [str]) ->
          let* next = IO.next stream in
          begin match next with
          | Some `End_element ->
             begin match int_of_string_opt str with
             | Some int -> IO.return (`Int int)
             | None -> raise (Parse_error ("Malformed int " ^ str))
             end
          | _ -> expected_closing ()
          end
       | _ -> raise (Parse_error "Expected int")
       end
    | Some (`Start_element((_, "real"), _)) ->
       let* next = IO.next stream in
       begin match next with
       | Some (`Text [str]) ->
          let* next = IO.next stream in
          begin match next with
          | Some `End_element ->
             begin match Float.of_string_opt str with
             | Some float -> IO.return (`Float float)
             | None -> raise (Parse_error ("Malformed float " ^ str))
             end
          | _ -> expected_closing ()
          end
       | _ -> raise (Parse_error "Expected float")
       end
    | Some (`Start_element((_, "string"), _)) ->
       let* next = IO.next stream in
       begin match next with
       | Some (`Text [str]) ->
          let* next = IO.next stream in
          begin match next with
          | Some `End_element -> IO.return (`String str)
          | _ -> expected_closing ()
          end
       (* Empty string *)
       | Some `End_element -> IO.return (`String "")
       | _ -> raise (Parse_error "Expected text inside string")
       end
    | Some (`Start_element((_, "true"), _)) ->
       let* next = IO.next stream in
       begin match next with
       | Some `End_element -> IO.return (`Bool true)
       | _ -> expected_closing ()
       end
    | Some (`Start_element((_, start), _)) ->
       raise (Parse_error ("Got unknown element " ^ start))
    | Some `End_element -> raise (Parse_error "Got end element")
    | None -> end_of_doc ()

  let plist_of_stream_exn stream =
    let* next = skip_whitespace stream in
    match next with
    | Some (`Start_element((_, "plist"), _)) ->
       let* ret = parse_val stream in
       let* next = skip_whitespace stream in
       begin match next with
       | Some `End_element -> IO.return ret
       | _ -> expected_closing ()
       end
    | _ -> raise (Parse_error "Expected opening plist")

  let parse_exn ?report ?encoding ?namespace ?entity ?context s =
    IO.parse_xml ?report ?encoding ?namespace ?entity ?context s
    |> Markup.signals
    |> Markup.content
    |> plist_of_stream_exn
end

include Make(IO)
