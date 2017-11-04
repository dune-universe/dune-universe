module Debug_in_this_directory = Debug
open Core
open Core_extended.Std
module Debug = Debug_in_this_directory
open Re2.Std
module Crypto = Crypto.Cryptokit
module Hash = Crypto.Hash

module rec Multipart : sig
  type t =
    { boundary : Boundary.t
    ; prologue : Bigstring_shared.t option
    ; epilogue : Bigstring_shared.t option
    ; parts    : Message.t list
    } [@@deriving sexp, compare, hash]

  (* Returns none if this is not a multipart message. *)
  val of_bigstring_shared
    :  media_type:Media_type.t
    -> boundary:Boundary.t
    -> Bigstring_shared.t
    -> t Or_error.t

  val map_data
    :  t
    -> f:(Octet_stream.t -> Octet_stream.t)
    -> t

  include String_monoidable.S with type t := t
end = struct
  type t = {
    boundary : Boundary.t;
    prologue : Bigstring_shared.t sexp_option;
    epilogue : Bigstring_shared.t sexp_option;
    parts    : Message.t list;
  } [@@deriving sexp, compare, hash]

  let of_bigstring_shared ~media_type ~boundary bstr =
    let open Or_error.Monad_infix in
    let prologue, parts, epilogue = Boundary.split boundary bstr in
    Debug.run_debug (fun () ->
      eprintf "Boundary: %s; Part count: %d\n"
        (Boundary.to_string boundary) (List.length parts));
    List.map parts ~f:(fun part ->
      Or_error.tag
        (Message.of_bigstring_shared ~parent:(Some media_type) part)
        ~tag:(sprintf "failed part:\n%s" (Bigstring_shared.to_string part)))
    |> Or_error.all
    >>= fun parts ->
    Ok
      { boundary
      ; prologue
      ; epilogue
      ; parts
      }
  ;;

  let map_data t ~f =
    { t with parts = List.map t.parts ~f:(Message.map_data ~f) }
  ;;

  let to_string_monoid t =
    let boundary = Boundary.generate ~suggest:t.boundary () in
    Boundary.join boundary
      ( t.prologue
      , List.map t.parts ~f:Message.to_string_monoid
      , t.epilogue)
end
and Content : sig
  type t =
      Multipart of Multipart.t
    | Data of Octet_stream.t
  [@@deriving sexp, compare, hash]
  ;;

  val empty : unit -> t

  val of_bigstring_shared :
    headers:Headers.t
    -> parent:(Media_type.t option)
    -> Bigstring_shared.t
    -> t Or_error.t

  val map_data
    :  t
    -> f:(Octet_stream.t -> Octet_stream.t)
    -> t

  val of_data : Octet_stream.t -> t
  val of_multipart : boundary:Boundary.t -> Message.t list -> t

  include String_monoidable.S with type t := t
end
= struct
  (* Message and multipart hold no encoding, as they must be encoded using
     7bit encoding with US-ASCII character set *)
  type t = Multipart of Multipart.t
         | Data of Octet_stream.t
  [@@deriving sexp, compare, hash]
  ;;

  let empty () = Data Octet_stream.empty

  let of_bigstring_shared ~headers ~parent bstr =
    let open Or_error.Monad_infix in
    let media_type =
      Option.value (Media_type.last headers)
        ~default:(Media_type.default ~parent)
    in
    let encoding = Octet_stream.Encoding.of_headers_or_default headers in
    let octet_stream = Octet_stream.create ~encoding bstr in
    let decode octet_stream =
      match Octet_stream.decode octet_stream with
      | None ->
        Or_error.error "Unknown message encoding"
          encoding Octet_stream.Encoding.sexp_of_t
      | Some decoded_bstr -> Ok decoded_bstr
    in
    match Media_type.multipart_boundary media_type with
    | Some boundary ->
      (* According to Wikipedia, the content-transfer-encoding of a multipart
         type must always be "7bit", "8bit" or "binary" to avoid the
         complications that would be posed by multiple levels of decoding. In
         this case this decode call is free. *)
      decode octet_stream
      >>= fun decoded_bstr ->
      Multipart.of_bigstring_shared ~media_type ~boundary decoded_bstr
      >>= fun multipart ->
      Ok (Multipart multipart)
    | None ->
      Ok (Data octet_stream)
  ;;

  let map_data t ~f =
    match t with
    | Multipart t ->
      Multipart (Multipart.map_data t ~f)
    | Data data ->
      Data (f data)
  ;;

  let to_string_monoid = function
    | Multipart multipart -> Multipart.to_string_monoid multipart
    | Data octet_stream ->
      Octet_stream.to_string_monoid octet_stream
  ;;

  let of_data octet_stream = Data octet_stream
  let of_multipart ~boundary parts =
    Multipart { boundary; prologue=None; epilogue=None; parts }
end
and Message : sig
  type t [@@deriving sexp, compare, hash]

  val empty : unit -> t

  val create : headers:Headers.t -> content:Content.t -> t

  val of_bigstring_shared
    :  parent:(Media_type.t option)
    -> Bigstring_shared.t
    -> Message.t Or_error.t

  val to_bigstring_shared : t -> Bigstring_shared.t

  include String_monoidable.S with type t := t
  include Stringable.S with type t := t
  val of_bigstring : Bigstring.t -> t Or_error.t
  val to_bigstring : t -> Bigstring.t

  val headers : t -> Headers.t

  val last_header : ?whitespace:Headers.Whitespace.t -> t -> Headers.Name.t -> Headers.Value.t option
  val find_all_headers : ?whitespace:Headers.Whitespace.t -> t -> Headers.Name.t -> Headers.Value.t list

  val set_headers : t -> Headers.t -> t

  val modify_headers : t -> f:(Headers.t -> Headers.t) -> t

  val add_header : ?whitespace:Headers.Whitespace.t -> t -> name:string -> value:string -> t
  val add_header_at_bottom : ?whitespace:Headers.Whitespace.t -> t -> name:string -> value:string -> t

  val set_header : ?whitespace:Headers.Whitespace.t -> t -> name:string -> value:string -> t
  val set_header_at_bottom : ?whitespace:Headers.Whitespace.t -> t -> name:string -> value:string -> t

  val add_headers : ?whitespace:Headers.Whitespace.t -> t -> (string * string) list -> t
  val add_headers_at_bottom : ?whitespace:Headers.Whitespace.t -> t -> (string * string) list -> t

  val filter_headers : ?whitespace:Headers.Whitespace.t -> t -> f:(name:Headers.Name.t -> value:Headers.Value.t -> bool) -> t
  val map_headers : ?whitespace:Headers.Whitespace.t -> t -> f:(name:Headers.Name.t -> value:Headers.Value.t -> string) -> t

  val content : t -> Content.t

  val set_content : t -> Content.t -> t

  val map_data
    : t -> f:(Octet_stream.t -> Octet_stream.t) -> t

  val raw_content : t -> Bigstring_shared.t
end = struct
  type t =
    { headers    : Headers.t
    ; line_break : bool
    ; content    : Content.t
    } [@@deriving sexp, compare, hash]
  ;;

  open Or_error.Monad_infix

  let create ~headers ~content =
    { headers; line_break=true; content }

  let empty () =
    { headers = Headers.empty
    ; line_break = true
    ; content = Content.empty ()
    }

  let of_grammar ~parent bstr (`Message (headers, content_offset)) =
    let headers = Headers.of_list ~whitespace:`Raw headers in
    let line_break, bstr =
      match content_offset with
      | `Truncated ->
        Debug.run_debug (fun () -> eprintf "Warning: Message truncated\n%!");
        false, Bigstring_shared.of_string ""
      | `Bad_headers pos ->
        false, Bigstring_shared.sub ~pos bstr
      | `Content_offset pos ->
        true, Bigstring_shared.sub ~pos bstr
    in
    Content.of_bigstring_shared ~headers ~parent bstr
    >>= fun content ->
    Ok { headers; line_break; content }
  ;;

  (* The default type of a message depends on the type of its parent,
     so we need to pass it around. *)
  let of_bigstring_shared ~parent bstr =
    let lexbuf = Bigstring_shared.to_lexbuf bstr in
    begin
      try Ok (Email_grammar.message
                (Email_lexer.message (Email_lexer_state.create ())) lexbuf)
      with _ ->
        (* Looks like lexer just throws Failure, not Parsing.Parse_error *)
        let pos = lexbuf.Lexing.lex_curr_p in
        Or_error.error_string
          (sprintf "Error parsing email at line %d, column %d"
             pos.Lexing.pos_lnum
             (pos.Lexing.pos_cnum - pos.Lexing.pos_bol))
    end
    >>= fun parse_result ->
    of_grammar ~parent bstr parse_result
  ;;

  let of_string str =
    of_bigstring_shared ~parent:None (Bigstring_shared.of_string str)
    |> Or_error.ok_exn
  ;;

  let of_bigstring bstr =
    of_bigstring_shared ~parent:None (Bigstring_shared.of_bigstring bstr)

  let map_data t ~f =
    { t with content = Content.map_data ~f t.content }
  ;;

  let raw_content t =
    Bigstring_shared.of_string_monoid (Content.to_string_monoid t.content)

  let to_string_monoid t =
    let sep = if t.line_break then String_monoid.nl else String_monoid.empty in
    String_monoid.concat ~sep [
      Headers.to_string_monoid t.headers;
      Content.to_string_monoid t.content
    ]
  ;;

  let to_string t = String_monoid.to_string (to_string_monoid t);;
  let to_bigstring t = String_monoid.to_bigstring (to_string_monoid t);;
  let to_bigstring_shared t =
    Bigstring_shared.of_string_monoid (to_string_monoid t);;

  let headers t = t.headers;;
  let content t = t.content;;
  let set_content t content = { t with content };;
  let set_headers t headers = { t with headers };;

  let modify_headers t ~f =
    headers t |> f |> set_headers t

  let last_header ?whitespace t name = Headers.last ?whitespace (headers t) name
  let find_all_headers ?whitespace t name = Headers.find_all ?whitespace (headers t) name

  let add_header ?whitespace t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.add ?whitespace headers ~name ~value)

  let add_header_at_bottom ?whitespace t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.add_at_bottom ?whitespace headers ~name ~value)

  let set_header ?whitespace t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.set ?whitespace headers ~name ~value)

  let set_header_at_bottom ?whitespace t ~name ~value =
    modify_headers t ~f:(fun headers ->
      Headers.set_at_bottom ?whitespace headers ~name ~value)

  let add_headers ?whitespace t ts =
    modify_headers t ~f:(fun headers ->
      Headers.add_all ?whitespace headers ts)

  let add_headers_at_bottom ?whitespace t ts =
    modify_headers t ~f:(fun headers ->
      Headers.add_all_at_bottom ?whitespace headers ts)

  let filter_headers ?whitespace t ~f =
    modify_headers t ~f:(fun headers ->
      Headers.filter ?whitespace headers ~f)

  let map_headers ?whitespace t ~f =
    modify_headers t ~f:(fun headers ->
      Headers.map ?whitespace headers ~f)
end

include Message
include Comparable.Make(Message)

include Binable.Of_binable (Bigstring)
    (struct
      type nonrec t = t
      let to_binable = to_bigstring
      let of_binable bs = of_bigstring bs |> Or_error.ok_exn
    end)

let of_bigbuffer buffer =
  of_bigstring (Bigbuffer.big_contents buffer)

type email = t [@@deriving bin_io, sexp_of]

module Simple = struct
  let make_id () =
    sprintf !"<%s/%s+%{Uuid}@%s>"
      (Unix.getlogin ())
      (Sys.executable_name |> Filename.basename)
      (Uuid.create ())
      (Unix.gethostname ())

  let utc_offset_string time ~zone =
    let utc_offset   = Time.utc_offset time ~zone in
    let is_utc       = Time.Span.(=) utc_offset Time.Span.zero in
    if is_utc
    then "Z"
    else
      String.concat
        [ (if Time.Span.(<) utc_offset Time.Span.zero then "-" else "+");
          Time.Ofday.to_string_trimmed
            (Time.Ofday.of_span_since_start_of_day (Time.Span.abs utc_offset));
        ]

  let rfc822_date now =
    let zone = (force Time.Zone.local) in
    let offset_string =
      utc_offset_string ~zone now
      |> String.filter ~f:(fun c -> Char.(<>) c ':')
    in
    let now_string = Time.format now "%a, %d %b %Y %H:%M:%S" ~zone in
    sprintf "%s %s" now_string offset_string

  let bigstring_shared_to_file data file =
    let open Async in
    Deferred.Or_error.try_with (fun () ->
      Writer.with_file file
        ~f:(fun w ->
          String_monoid.output_unix (Bigstring_shared.to_string_monoid data) w;
          Writer.flushed w))

  module Expert = struct
    let content ~whitespace ~extra_headers ~encoding body =
      let headers =
        [ "Content-Transfer-Encoding",
          (Octet_stream.Encoding.to_string (encoding :> Octet_stream.Encoding.t))
        ] @ extra_headers
      in
      let headers = Headers.of_list ~whitespace headers in
      let content =
        body
        |> Bigstring_shared.of_string
        |> Octet_stream.encode ~encoding
        |> Content.of_data
      in
      Message.create ~headers ~content

    let multipart ~whitespace ~content_type ~extra_headers parts =
      let boundary = Boundary.generate () in
      let headers =
        [ "Content-Type", (sprintf !"%s; boundary=\"%{Boundary}\"" content_type boundary)
        ] @ extra_headers
      in
      let headers = Headers.of_list ~whitespace headers in
      let content = Content.of_multipart ~boundary parts in
      Message.create ~headers ~content

    let create_raw
          ?(from=Email_address.local_address () |> Email_address.to_string)
          ~to_
          ?(cc=[])
          ?(reply_to=[])
          ~subject
          ?id
          ?in_reply_to
          ?date
          ?auto_generated
          ?(extra_headers=[])
          ?(attachments=[])
          content
      =
      let id = match id with
        | None ->  make_id ()
        | Some id -> id
      in
      let date = match date with
        | None -> rfc822_date (Time.now ())
        | Some date -> date
      in
      let headers =
        extra_headers
        @ [ "From", from ]
        @ (if List.is_empty to_ then []
           else [ "To", String.concat to_ ~sep:",\n\t" ])
        @ (if List.is_empty cc then []
           else [ "Cc", String.concat cc ~sep:",\n\t" ])
        @ (if List.is_empty reply_to then []
           else [ "Reply-To", String.concat reply_to ~sep:",\n\t" ])
        @ [ "Subject", subject ]
        @ [ "Message-Id", id ]
        @ (match in_reply_to with
          | None -> []
          | Some in_reply_to -> [ "In-Reply-To", in_reply_to ])
        @ (match auto_generated with
          | None -> []
          | Some () ->
            [ "Auto-Submitted", "auto-generated"
            ; "Precedence", "bulk" ])
        @ [ "Date", date ]
      in
      match attachments with
      | [] ->
        add_headers content headers
      | attachments ->
        multipart
          ~whitespace:`Normalize
          ~content_type:"multipart/mixed"
          ~extra_headers:headers
          ((set_header_at_bottom content
              ~name:"Content-Disposition" ~value:"inline")
           :: List.map attachments ~f:(fun (name,content) ->
             let content_type =
               last_header content "Content-Type"
               |> Option.value ~default:"application/x-octet-stream"
             in
             content
             |> set_header_at_bottom
                  ~name:"Content-Type" ~value:(sprintf "%s; name=%s" content_type (Mimestring.quote name))
             |> set_header_at_bottom
                  ~name:"Content-Disposition" ~value:(sprintf "attachment; filename=%s" (Mimestring.quote name))))

    let%test_unit _ =
      let t =
        content
          ~whitespace:`Raw
          ~encoding:`Quoted_printable
          ~extra_headers:[ "header1", "value1"
                         ; "header2", "value2"]
          "x"
      in
      [%test_result: string] (to_string t)
        ~expect:"Content-Transfer-Encoding:quoted-printable\
                 \nheader1:value1\
                 \nheader2:value2\
                 \n\nx"

    let%test_unit _ =
      let t =
        content
          ~whitespace:`Normalize
          ~encoding:`Quoted_printable
          ~extra_headers:[]
          "x\n"
      in
      [%test_result: string] (to_string t)
        ~expect:"Content-Transfer-Encoding: quoted-printable\n\nx\n"
  end

  module Mimetype = struct
    type t = string
    let text = "text/plain"
    let html = "text/html"
    let pdf = "application/pdf"
    let jpg = "image/jpeg"
    let png = "image/png"

    let multipart_mixed = "multipart/mixed"
    let multipart_alternative = "multipart/alternative"
    let multipart_related = "multipart/related"

    let from_extension ext =
      Magic_mime_external.Mime_types.map_extension ext

    let from_filename file =
      Magic_mime_external.Magic_mime.lookup file

    let guess_encoding : t -> Octet_stream.Encoding.known = function
      | "text/plain"
      | "text/html" -> `Quoted_printable
      | _ -> `Base64
  end

  type attachment_name = string

  module Attachment = struct
    type t =
      { filename : string
      (* These are expensive operations. Ensure they are only computed once, and
         lazily. *)
      ; raw_data : Bigstring_shared.t Or_error.t Lazy.t
      ; md5      : string Or_error.t Lazy.t
      ; email    : email
      }

    let filename t = t.filename
    let email t    = t.email
    let raw_data t = Lazy.force t.raw_data
    let md5 t      = Lazy.force t.md5

    let to_hex digest =
      let result = String.create (String.length digest * 2) in
      let hex = "0123456789ABCDEF" in
      for i = 0 to String.length digest - 1 do
        let c = int_of_char digest.[i] in
        result.[2*i] <- hex.[c lsr 4];
        result.[2*i+1] <- hex.[c land 0xF]
      done;
      result

    let create ~email ~filename ~content =
      let raw_data =
        lazy (
          Or_error.try_with (fun () ->
            Octet_stream.decode content
            |> Option.value_exn)
        )
      in
      let md5 =
        lazy (
          match Lazy.force raw_data with
          | Error _ as err -> err
          | Ok data ->
            Or_error.try_with (fun () ->
              Crypto.hash_string (Hash.md5 ())
                (Bigstring_shared.to_string data)
              |> to_hex)
        )
      in
      { filename; raw_data; md5; email }

    let to_file t file =
      match raw_data t with
      | Error _ as err -> Async.return err
      | Ok data -> bigstring_shared_to_file data file
  end

  module Content = struct
    type t = email [@@deriving bin_io, sexp_of]
    let of_email = ident

    let create ~content_type ?(encoding=Mimetype.guess_encoding content_type) ?(extra_headers=[]) content =
      Expert.content
        ~whitespace:`Normalize
        ~extra_headers:(extra_headers @ [ "Content-Type", content_type ])
        ~encoding
        content

    let of_file ?content_type ?encoding ?extra_headers file =
      let open Async in
      Reader.file_contents file
      >>| fun content ->
      let content_type = match content_type with
        | None -> Mimetype.from_filename file
        | Some content_type -> content_type
      in
      create ~content_type ?encoding ?extra_headers content

    let html ?(encoding=`Quoted_printable) ?extra_headers content =
      create ?extra_headers ~content_type:Mimetype.html ~encoding content

    let text ?(encoding=`Quoted_printable) ?extra_headers content =
      create ?extra_headers ~content_type:Mimetype.text ~encoding content

    let create_multipart ?(extra_headers=[]) ~content_type = function
      | [] -> failwith "at least one part is required"
      | [content] -> add_headers content extra_headers
      | parts ->
        Expert.multipart
          ~whitespace:`Normalize
          ~content_type
          ~extra_headers
          parts

    let alternatives ?extra_headers =
      create_multipart ?extra_headers ~content_type:Mimetype.multipart_alternative

    let mixed ?extra_headers =
      create_multipart ?extra_headers ~content_type:Mimetype.multipart_mixed

    let with_related ?(extra_headers=[]) ~resources t =
      Expert.multipart
        ~whitespace:`Normalize
        ~content_type:Mimetype.multipart_related
        ~extra_headers
        (add_headers t
           [ "Content-Disposition", "inline" ]
         :: List.map resources ~f:(fun (name,content) ->
           add_headers content
             [ "Content-Id", sprintf "<%s>" name ]))

    let parse_last_header t name =
      match last_header t name with
      | None -> None
      | Some str ->
        match String.split str ~on:';'
              |> List.map ~f:String.strip with
        | [] -> None
        | v::args ->
          let args = List.map args ~f:(fun str ->
            match String.lsplit2 str ~on:'=' with
            | None -> str, None
            | Some (k,v) -> String.strip k, Some (String.strip v))
          in
          Some (v,args)

    let content_type t =
      let open Option.Monad_infix in
      (parse_last_header t "Content-Type" >>| fst)
      |> Option.value ~default:"application/x-octet-stream"

    let attachment_name t =
      let open Option.Monad_infix in
      let quoted_re = Re2.create "\"(.*)\"" in
      let unquote name =
        let f name =
          match
            Or_error.try_with (fun () ->
              Re2.replace_exn (Or_error.ok_exn quoted_re) name ~f:(fun match_ ->
                Re2.Match.get_exn ~sub:(`Index 1) match_))
          with
          | Error _ -> name
          | Ok name -> name
        in
        Option.map name ~f
      in
      Option.first_some
        begin
          parse_last_header t "Content-Disposition"
          >>= fun (disp, args) ->
          if String.Caseless.equal disp "attachment" then
            List.find_map args ~f:(fun (k,v) ->
              if String.Caseless.equal k "filename" then (unquote v) else None)
          else
            None
        end
        begin
          parse_last_header t "Content-Type"
          >>= fun (_, args) ->
          List.find_map args ~f:(fun (k,v) ->
            if String.Caseless.equal k "name" then (unquote v) else None)
        end

    let related_part_cid t =
      let open Option.Monad_infix in
      last_header t "Content-Id"
      >>| String.strip
      >>| fun str ->
      begin
        String.chop_prefix str ~prefix:"<"
        >>= String.chop_suffix ~suffix:">"
      end |> Option.value ~default:str

    let content_disposition t =
      match parse_last_header t "Content-Disposition" with
      | None -> `Inline
      | Some (disp,_) ->
        if String.Caseless.equal disp "inline" then `Inline
        else if String.Caseless.equal disp "attachment" then
          `Attachment (attachment_name t |> Option.value ~default:"unnamed-attachment")
        else match attachment_name t with
          | None -> `Inline
          | Some name -> `Attachment name

    let parts t =
      match content t with
      | Content.Multipart ts -> Some ts.Multipart.parts
      | Content.Data _ -> None

    let content t =
      match content t with
      | Content.Multipart _ -> None
      | Content.Data data -> Some data

    let rec inline_parts t =
      match parts t with
      | Some parts ->
        if String.Caseless.equal (content_type t) Mimetype.multipart_alternative then
          (* multipart/alternative is special since an aplication is expected to
             present/process any one of the alternative parts. The logic for picking
             the 'correct' alternative is application dependant so leaving this to
             to users (e.g. first one that parses) *)
          [t]
        else
          List.concat_map parts ~f:inline_parts
      | None ->
        match content_disposition t with
        | `Inline -> [t]
        | `Attachment _ -> []

    let rec alternative_parts t =
      match parts t with
      | None -> [t]
      | Some ts ->
        if String.Caseless.equal (content_type t) Mimetype.multipart_alternative then
          List.concat_map ts ~f:alternative_parts
        else [t]

    let rec all_related_parts t =
      let get_cid t =
        Option.map (related_part_cid t) ~f:(fun cid -> [cid, t])
        |> Option.value ~default:[]
      in
      (get_cid t) @
      (parts t
       |> Option.value ~default:[]
       |> List.concat_map ~f:all_related_parts)

    let find_related t name =
      List.find (all_related_parts t) ~f:(fun (cid, _t) ->
        String.equal cid name)
      |> Option.map ~f:snd

    let rec all_attachments t =
      match content_disposition t with
      | `Attachment filename ->
        begin match content t with
        (* It is an error if the email is of content type multipart. So just return [] *)
        | None -> []
        | Some content -> [Attachment.create ~email:t ~filename ~content]
        end
      | `Inline ->
        parts t
        |> Option.value ~default:[]
        |> List.concat_map ~f:all_attachments

    let find_attachment t name =
      List.find (all_attachments t) ~f:(fun attachment ->
        String.equal (Attachment.filename attachment) name)

    let to_file t file =
      let open Async in
      match content t with
      | None -> Deferred.Or_error.errorf "The payload of this email is ambigous, you
                  you should decompose the email further"
      | Some content ->
        match Octet_stream.decode content with
        | None -> Deferred.Or_error.errorf "The message payload used an unknown encoding"
        | Some content -> bigstring_shared_to_file content file
  end

  type t = email [@@deriving sexp_of]

  let create
        ?from
        ~to_
        ?cc
        ?reply_to
        ~subject
        ?id
        ?in_reply_to
        ?date
        ?auto_generated
        ?extra_headers
        ?attachments
        content
    =
    Expert.create_raw
      ?from:(Option.map from ~f:Email_address.to_string)
      ~to_:(List.map to_ ~f:Email_address.to_string)
      ?cc:(Option.map cc ~f:(List.map ~f:Email_address.to_string))
      ?reply_to:(Option.map reply_to ~f:(List.map ~f:Email_address.to_string))
      ~subject
      ?id
      ?in_reply_to
      ?date:(Option.map date ~f:rfc822_date)
      ?auto_generated
      ?extra_headers
      ?attachments
      content

  let decode_last_header name ~f t =
    Option.bind
      (last_header ~whitespace:`Normalize t name)
      ~f:(fun v -> Option.try_with (fun () -> f v))

  let from =
    decode_last_header "From" ~f:Email_address.of_string_exn

  let to_ =
    decode_last_header "To" ~f:Email_address.list_of_string_exn

  let cc =
    decode_last_header "Cc" ~f:Email_address.list_of_string_exn

  let subject =
    decode_last_header "Subject" ~f:Fn.id

  let id =
    decode_last_header "Message-Id" ~f:Fn.id


  let all_attachments = Content.all_attachments
  let find_attachment = Content.find_attachment
  let all_related_parts = Content.all_related_parts
  let find_related = Content.find_related
  let inline_parts =  Content.inline_parts

  let rec map_attachments t ~f =
    let open Async in
    match content t with
    | Data _data ->
      begin match all_attachments t with
      | [] -> Async.return t
      | [attachment] -> f attachment
      | _::_ -> failwith "impossible"
      end
    | Multipart mp ->
      Deferred.List.map mp.Multipart.parts ~f:(map_attachments ~f)
      >>| fun parts' ->
      let mp' = {mp with Multipart.parts = parts'} in
      set_content t (Multipart mp')
end


let%test_module _ =
  (module struct
    let check s =
      let b = Bigbuffer.create 1000 in
      Bigbuffer.add_string b s;
      let result =
        Bigbuffer.big_contents b
        |> of_bigstring
        |> Or_error.ok_exn
        |> to_string
      in
      [%test_result: string] ~expect:s result

    (* A message without headers must start with an empty line. *)
    let%test_unit _ = check "\n"
    let%test_unit _ = check "\nhello world"
    let%test_unit _ = check "\nhello world\n hello again\n"
  end)
