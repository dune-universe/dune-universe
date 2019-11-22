open! Core

module Multipart : sig
  type t = private
    { boundary : Boundary.t
    ; prologue : Bigstring_shared.t option
    ; epilogue : Bigstring_shared.t option
    ; parts : Email.t list
    ; container_headers : Headers.t
    }
  [@@deriving fields, sexp_of]

  val create_unsafe
    :  boundary:Boundary.t
    -> ?prologue:Bigstring_shared.t
    -> ?epilogue:Bigstring_shared.t
    -> Email.t list
    -> container_headers:Headers.t
    -> t

  val create
    :  ?boundary:Boundary.t
    -> ?prologue:Bigstring_shared.t
    -> ?epilogue:Bigstring_shared.t
    -> ?container_headers:Headers.t
    -> Email.t list
    -> t

  val set
    :  t
    -> ?boundary:Boundary.t
    -> ?prologue:Bigstring_shared.t option
    -> ?epilogue:Bigstring_shared.t option
    -> ?parts:Email.t list
    -> ?container_headers:Headers.t
    -> unit
    -> t

  include String_monoidable.S with type t := t
end = struct
  type t =
    { boundary : Boundary.t
    ; prologue : Bigstring_shared.t option
    ; epilogue : Bigstring_shared.t option
    ; parts : Email.t list
    ; container_headers : Headers.t
    }
  [@@deriving fields, sexp_of]

  let create_unsafe ~boundary ?prologue ?epilogue parts ~container_headers =
    { boundary; prologue; epilogue; parts; container_headers }
  ;;

  let create ?boundary ?prologue ?epilogue ?(container_headers = Headers.empty) parts =
    let boundary =
      Boundary.generate_non_conflicting_boundary
        ?prologue
        ~parts:(List.map parts ~f:Email.to_string_monoid)
        ?epilogue
        (Option.value_map
           boundary
           ~default:Boundary.Generator.default
           ~f:Boundary.Generator.from_existing_boundary)
    in
    create_unsafe ~boundary ?prologue ?epilogue parts ~container_headers
  ;;

  let set
        t
        ?(boundary = t.boundary)
        ?(prologue = t.prologue)
        ?(epilogue = t.epilogue)
        ?(parts = t.parts)
        ?(container_headers = t.container_headers)
        ()
    =
    create ~boundary ?prologue ?epilogue ~container_headers parts
  ;;

  let to_string_monoid t =
    Boundary.join_without_checking_for_conflicts
      ?prologue:t.prologue
      ~parts:(List.map t.parts ~f:Email.to_string_monoid)
      ?epilogue:t.epilogue
      t.boundary
  ;;
end

type t =
  | Multipart of Multipart.t
  | Message of Email.t
  | Data of Octet_stream.t
[@@deriving sexp_of]

let rec multipart_of_bigstring_shared ~boundary ~container_headers bstr =
  let open Or_error.Let_syntax in
  let prologue, parts, epilogue = Boundary.split boundary bstr in
  let%map parts =
    List.map parts ~f:(fun part ->
      Or_error.tag
        (Or_error.try_with (fun () ->
           Email.of_bigstring (Bigstring_shared.to_bigstring part)))
        ~tag:(sprintf "failed part:\n%s" (Bigstring_shared.to_string part)))
    |> Or_error.all
  in
  Multipart.create_unsafe ~boundary ?prologue ?epilogue ~container_headers parts

and content_of_bigstring_shared ~headers ?container_headers bstr =
  let open Or_error.Let_syntax in
  let parent_media_type = Option.bind container_headers ~f:Media_type.from_headers in
  let media_type =
    Option.value
      (Media_type.from_headers headers)
      ~default:(Media_type.default ?parent:parent_media_type ())
  in
  let encoding = Octet_stream.Encoding.of_headers_or_default headers in
  let octet_stream = Octet_stream.of_bigstring_shared ~encoding bstr in
  let decode octet_stream =
    match Octet_stream.decode octet_stream with
    | None ->
      Or_error.error "Unknown message encoding" encoding Octet_stream.Encoding.sexp_of_t
    | Some decoded_bstr -> Ok decoded_bstr
  in
  match Media_type.multipart_boundary media_type with
  | Some boundary ->
    (* According to Wikipedia, the content-transfer-encoding of a multipart
       type must always be "7bit", "8bit" or "binary" to avoid the
       complications that would be posed by multiple levels of decoding. In
       this case this decode call is free. *)
    let%bind decoded_bstr = decode octet_stream in
    let%bind multipart =
      multipart_of_bigstring_shared ~boundary ~container_headers:headers decoded_bstr
    in
    Ok (Multipart multipart)
  | None ->
    if Media_type.is_message_rfc822 media_type
    then (
      let%bind decoded_bstr = decode octet_stream in
      let%bind email =
        Or_error.try_with (fun () ->
          Email.of_bigstring (Bigstring_shared.to_bigstring decoded_bstr))
      in
      Ok (Message email))
    else Ok (Data octet_stream)

and parse ?container_headers email =
  content_of_bigstring_shared
    ?container_headers
    ~headers:(Email.headers email)
    (Email.raw_content email |> Email_raw_content.to_bigstring_shared)
;;

let to_string_monoid = function
  | Multipart multipart -> Multipart.to_string_monoid multipart
  | Message message -> Email.to_string_monoid message
  | Data octet_stream ->
    Octet_stream.encoded_contents octet_stream |> Bigstring_shared.to_string_monoid
;;

let to_bigstring_shared t =
  to_string_monoid t |> String_monoid.to_bigstring |> Bigstring_shared.of_bigstring
;;

let to_raw_content t = to_bigstring_shared t |> Email_raw_content.of_bigstring_shared

let rec multipart_map_data ~on_unparsable_content mp ~f =
  Multipart.set
    mp
    ~parts:(List.map mp.Multipart.parts ~f:(map_data ~on_unparsable_content ~f))
    ()

and content_map_data ~on_unparsable_content t ~f =
  match t with
  | Multipart t -> Multipart (multipart_map_data ~on_unparsable_content t ~f)
  | Message message -> Message (map_data ~on_unparsable_content message ~f)
  | Data data -> Data (f data)

and map_data ~on_unparsable_content email ~f =
  match parse email with
  | Ok content ->
    let content = content_map_data content ~on_unparsable_content ~f in
    Email.set_raw_content
      email
      (to_bigstring_shared content |> Email_raw_content.of_bigstring_shared)
  | Error e ->
    (match on_unparsable_content with
     | `Skip -> email
     | `Raise -> raise_s [%message "[map_data] has unparsable content" (e : Error.t)])
;;

let map_data ?(on_unparsable_content = `Skip) email ~f =
  map_data ~on_unparsable_content email ~f
;;

let to_email ~headers t =
  let headers =
    let media_type =
      match t with
      | Multipart mp ->
        (match Media_type.from_headers headers with
         | None -> Some (Media_type.create_multipart "related" ~boundary:mp.boundary)
         | Some media_type ->
           Some (Media_type.set_multipart_boundary media_type mp.boundary))
      | _ -> None
    in
    match media_type with
    | None -> headers
    | Some media_type -> Media_type.set_headers headers media_type
  in
  Email.create
    ~headers
    ~raw_content:(to_bigstring_shared t |> Email_raw_content.of_bigstring_shared)
;;

let set_content email t = to_email ~headers:(Email.headers email) t
