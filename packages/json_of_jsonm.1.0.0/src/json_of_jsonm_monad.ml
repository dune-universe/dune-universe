module Error = struct
  type pos = { line : int; char : int }
  type t = { start : pos; end_ : pos; error : Jsonm_error.t }

  let range_of_jsom_range d =
    let ((start_line,start_char),(end_line,end_char)) = Jsonm.decoded_range d in
      ( { line = start_line; char = start_char }, { line = end_line; char = end_char }) 
  
  let of_jsonm_error d error =
    let (start, end_) = range_of_jsom_range d in { start; end_; error }
    
  let pos_to_string pos = Printf.sprintf "line %d, char %d" pos.line pos.char

  let to_string err =
    Printf.sprintf
      "error: %s from %s to %s"
      (Jsonm_error.to_string err.error)
      (pos_to_string err.start)
      (pos_to_string err.end_)

end

type json =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `List of json list
  | `Assoc of (string * json) list
  ]

module type IO = sig
  type 'a t

  val return : 'a -> 'a t
  val (>>=)  : 'a t -> ('a -> 'b t) -> 'b t
end

module type Json_encoder_decoder = sig
  module IO : IO

  type nonrec json = json

  val decode : reader:(Bytes.t -> int -> int IO.t) -> (json, string) result IO.t
  val decode_exn : reader:(Bytes.t -> int -> int IO.t) -> json IO.t
  val decode_string : string -> (json, string) result IO.t
  val decode_string_exn : string -> json IO.t

  val encode : writer:(Bytes.t -> int -> unit IO.t) -> json -> (unit, string) result IO.t
  val encode_exn : writer:(Bytes.t -> int -> unit IO.t) -> json -> unit IO.t
  val encode_string : json -> (string, string) result IO.t
  val encode_string_exn : json -> string IO.t
  val encode_string_hum : json -> (string, string) result IO.t
end

module Make(IO : IO) : Json_encoder_decoder with module IO := IO = struct
  open IO

  type nonrec json = json
  type reader = Bytes.t -> int -> int IO.t
  type writer = Bytes.t -> int -> unit IO.t

  let raise_of_error = function
    | Ok v -> v
    | Error s -> raise (Failure s)

  let (>>=?) a f =
    a >>= fun a ->
    match a with
    | Ok a -> f a
    | Error err -> return (Error err)

  let json_of_src ?encoding
      (src : [`Channel of in_channel | `String of string | `Manual of reader])
      : (json, string) result t
    =
    begin
      let dbuf = Bytes.create 4096 in
      let raise_error d error = Error (Error.of_jsonm_error d error) in
      let rec dec d = match Jsonm.decode d with
      | `Lexeme l -> return (Ok l)
      | `Error e -> raise_error d e |> return
      | `End -> assert false
      | `Await -> 
        match src with
        | `String _ | `Channel _ -> assert false
        | `Manual reader ->
          reader dbuf (Bytes.length dbuf)
          >>= fun rc ->
            Jsonm.Manual.src d dbuf 0 rc;
            dec d
      in
      let rec value v k d = match v with
      | `Os -> obj [] k d 
      | `As -> arr [] k d
      | `Null | `Bool _ | `String _ | `Float _ as v -> k v d
      | _ -> assert false
      and arr vs k d = dec d
        >>=? function
        | `Ae -> k (`List (List.rev vs)) d
        | v -> value v (fun v -> arr (v :: vs) k) d
      and obj ms k d =
        dec d
        >>=? function
        | `Oe -> k (`Assoc (List.rev ms)) d
        | `Name n ->
          dec d >>=? fun res -> value res (fun v -> obj ((n, v) :: ms) k) d
        | _ -> assert false
      in
      let d =
        match src with
        | `String _ | `Channel _ as src -> Jsonm.decoder ?encoding src
        | `Manual _ -> Jsonm.decoder ?encoding `Manual
      in
      dec d
      >>=? fun res ->
      value res (fun v _ -> return (Ok v)) d
    end
    >>= function
      | Ok res -> return (Ok res)
      | Error error -> Error (Error.to_string error) |> return
  ;;

  let decode ~reader = json_of_src (`Manual reader)
  let decode_exn ~reader = decode ~reader >>= fun res -> raise_of_error res |> return
  let decode_string s = json_of_src (`String s)
  let decode_string_exn s = json_of_src (`String s) >>= fun res -> raise_of_error res |> return

  let json_to_dst ~minify
      (dst : [`Channel of out_channel | `Buffer of Buffer.t | `Manual of writer])
      (json : json)
    =
    let check_float f =
      match classify_float f with
      | FP_infinite | FP_nan -> Error "invalid float encountered (NaN/Inf)" |> return
      | _ -> return (Ok ())
    in
    let dbuf = Bytes.create 4096 in
    let rec encode e l : (unit, string) result IO.t =
      match Jsonm.encode e l, dst with
      | `Partial, `Manual writer ->
        writer dbuf (Bytes.length dbuf - Jsonm.Manual.dst_rem e)
        >>= fun () ->
        Jsonm.Manual.dst e dbuf 0 (Bytes.length dbuf);
        encode e `Await
      | `Ok, _ -> return (Ok ())
      | _ -> assert false
    in
    let enc e l = encode e (`Lexeme l) in
    let rec value v k e = match v with
    | `List vs -> arr vs k e
    | `Assoc ms -> obj ms k e
    | `Null | `Bool _ | `String _ as v -> enc e v >>=? fun () -> k e
    | `Float f as v -> check_float f >>=? fun () -> enc e v >>=? fun () -> k e
    and arr vs k e = enc e `As >>=? fun () -> arr_vs vs k e
    and arr_vs vs k e = match vs with
    | v :: vs' -> value v (arr_vs vs' k) e
    | [] -> enc e `Ae >>=? fun () -> k e
    and obj ms k e = enc e `Os >>=? fun () -> obj_ms ms k e
    and obj_ms ms k e = match ms with
    | (n, v) :: ms -> enc e (`Name n) >>=? fun () -> value v (obj_ms ms k) e
    | [] -> enc e `Oe >>=? fun () -> k e
    in
    let e =
      match dst with
      | `Buffer _ | `Channel _ as dst -> Jsonm.encoder ~minify dst
      | `Manual _ ->
        let e = Jsonm.encoder ~minify `Manual in
        Jsonm.Manual.dst e dbuf 0 (Bytes.length dbuf);
        e
    in
    let () = match dst with
      | `Buffer _ | `Channel _ -> ()
      | `Manual _ -> Jsonm.Manual.dst e dbuf 0 (Bytes.length dbuf)
    in
    let finish e = encode e `End in
    value json finish e

  let encode ~writer json = json_to_dst ~minify:true (`Manual writer) json
  let encode_exn ~writer json =
    json_to_dst ~minify:true (`Manual writer) json
    >>= fun res -> raise_of_error res |> return

  let encode_string' ~minify json =
    let buf = Buffer.create 1024 in
    json_to_dst ~minify (`Buffer buf) json
    >>= function
      | Ok () -> Ok (Buffer.contents buf) |> return
      | Error err -> Error err |> return

  let encode_string json = encode_string' ~minify:true json
  let encode_string_exn json = encode_string' ~minify:true json
    >>= fun res -> raise_of_error res |> return

  let encode_string_hum json = encode_string' ~minify:false json
end
