open Core.Core_stable

module Stable = struct
  module Attachment = struct
    module Id = struct
      module V1 = struct
        type t =
          { filename : string
          ; path : int list
          }
        [@@deriving bin_io, compare, sexp]
      end
    end
  end

  module Content = struct
    module V1 = Email.Stable.V1
  end

  module Mimetype = struct
    module V1 = struct
      type t = string [@@deriving compare, sexp]
    end
  end
end

open! Core
open Poly
module Crypto = Crypto.Cryptokit
module Hash = Crypto.Hash

let make_id () =
  if am_running_inline_test
  then "{AUTO-GENERATED-ID}"
  else
    sprintf
      !"<%s/%s+%{Uuid}@%s>"
      (Unix.getlogin ())
      (Sys.executable_name |> Filename.basename)
      (Uuid_unix.create ())
      (Unix.gethostname ())
;;

let bigstring_shared_to_file data file =
  let open Async in
  Deferred.Or_error.try_with (fun () ->
    Writer.with_file file ~f:(fun w ->
      String_monoid.output_unix (Bigstring_shared.to_string_monoid data) w;
      Writer.flushed w))
;;

let last_header ?normalize t name = Headers.last ?normalize (Email.headers t) name

let add_headers t headers' =
  Email.modify_headers t ~f:(fun headers -> Headers.add_all headers headers')
;;

let set_header_at_bottom t ~name ~value =
  Email.modify_headers t ~f:(Headers.set_at_bottom ~name ~value)
;;

module Expert = struct
  let content ~normalize_headers ~extra_headers ~encoding body =
    let headers =
      [ ( "Content-Transfer-Encoding"
        , Octet_stream.Encoding.to_string (encoding :> Octet_stream.Encoding.t) )
      ]
      @ extra_headers
    in
    let headers = Headers.of_list ~normalize:normalize_headers headers in
    let octet_stream =
      body |> Bigstring_shared.of_string |> Octet_stream.encode ~encoding
    in
    Email_content.to_email ~headers (Data octet_stream)
  ;;

  let multipart ~normalize_headers ~content_type ~extra_headers parts =
    (* [Multipart.create] will generate a suitable boundary, and [to_email] will ensure
       that this is added to the [Content-Type] header. *)
    let multipart = Email_content.Multipart.create parts in
    let headers = [ "Content-Type", content_type ] @ extra_headers in
    let headers = Headers.of_list ~normalize:normalize_headers headers in
    Email_content.to_email ~headers (Multipart multipart)
  ;;

  let create_raw
        ?(from = Email_address1.local_address () |> Email_address.to_string)
        ~to_
        ?(cc = [])
        ?reply_to
        ~subject
        ?id
        ?in_reply_to
        ?date
        ?auto_generated
        ?(extra_headers = [])
        ?(attachments = [])
        content
    =
    let id =
      match id with
      | None -> make_id ()
      | Some id -> id
    in
    let date =
      match date with
      | None -> Email_date.rfc822_date (Time.now ())
      | Some date -> date
    in
    let headers =
      extra_headers
      @ [ "From", from ]
      @ (if List.is_empty to_ then [] else [ "To", String.concat to_ ~sep:",\n\t" ])
      @ (if List.is_empty cc then [] else [ "Cc", String.concat cc ~sep:",\n\t" ])
      @ (match reply_to with
        | None -> []
        | Some reply_to -> [ "Reply-To", reply_to ])
      @ [ "Subject", subject ]
      @ [ "Message-Id", id ]
      @ (match in_reply_to with
        | None -> []
        | Some in_reply_to -> [ "In-Reply-To", in_reply_to ])
      @ (match auto_generated with
        | None -> []
        | Some () -> [ "Auto-Submitted", "auto-generated"; "Precedence", "bulk" ])
      @ [ "Date", date ]
    in
    match attachments with
    | [] -> add_headers content headers
    | attachments ->
      multipart
        ~normalize_headers:`Whitespace
        ~content_type:"multipart/mixed"
        ~extra_headers:headers
        (set_header_at_bottom content ~name:"Content-Disposition" ~value:"inline"
         :: List.map attachments ~f:(fun (name, content) ->
           let content_type =
             last_header content "Content-Type"
             |> Option.value ~default:"application/x-octet-stream"
           in
           set_header_at_bottom
             content
             ~name:"Content-Type"
             ~value:(sprintf "%s; name=%s" content_type (Mimestring.quote name))
           |> set_header_at_bottom
                ~name:"Content-Disposition"
                ~value:(sprintf "attachment; filename=%s" (Mimestring.quote name))))
  ;;
end

module Mimetype = struct
  type t = Stable.Mimetype.V1.t [@@deriving compare, sexp_of]

  let text = "text/plain"
  let text_utf8 = "text/plain; charset=\"UTF-8\""
  let html = "text/html"
  let html_utf8 = "text/html; charset=\"UTF-8\""
  let pdf = "application/pdf"
  let jpg = "image/jpeg"
  let png = "image/png"
  let csv = "text/csv"
  let multipart_mixed = "multipart/mixed"
  let multipart_alternative = "multipart/alternative"
  let multipart_related = "multipart/related"
  let of_string t = t
  let equal = [%compare.equal: t]
  let arg_type = Command.Arg_type.create of_string
  let from_extension ext = Magic_mime_external.Mime_types.map_extension ext
  let from_filename file = Magic_mime_external.Magic_mime.lookup file

  let guess_encoding : t -> Octet_stream.Encoding.known = function
    | "text/plain" | "text/html" -> `Quoted_printable
    | _ -> `Base64
  ;;
end

type attachment_name = string

module Path : sig
  type t

  val root : t
  val child : t -> int -> t
  val to_int_list : t -> int list
end = struct
  type t = int list

  let root = []
  let child t i = i :: t
  let to_int_list t = List.rev t
end

module Attachment = struct
  module Id = struct
    type t = Stable.Attachment.Id.V1.t =
      { filename : string
      ; path : int list
      }
    [@@deriving compare, fields, sexp_of]
  end

  type t =
    { headers : Headers.t
    ; id : Id.t
    ; embedded_email : Email.t option
    (* These are expensive operations. Ensure they are only computed once, and
       lazily. *)
    ; raw_data : Bigstring_shared.t Or_error.t Lazy.t
    ; md5 : string Or_error.t Lazy.t
    ; sha256 : string Or_error.t Lazy.t
    }
  [@@deriving fields, sexp_of]

  let filename t = Id.filename t.id
  let raw_data t = Lazy.force t.raw_data
  let md5 t = Lazy.force t.md5
  let sha256 t = Lazy.force t.sha256

  let to_hex digest =
    let result = Bytes.create (String.length digest * 2) in
    let hex = "0123456789ABCDEF" in
    for i = 0 to String.length digest - 1 do
      let c = int_of_char digest.[i] in
      Bytes.set result (2 * i) hex.[c lsr 4];
      Bytes.set result ((2 * i) + 1) hex.[c land 0xF]
    done;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:result
  ;;

  let of_content' ?embedded_email ~headers ~filename ~path content =
    let raw_data =
      lazy
        (Or_error.try_with (fun () ->
           Octet_stream.decode (Lazy.force content) |> Option.value_exn))
    in
    let compute_hash ~hash =
      lazy
        (match Lazy.force raw_data with
         | Error _ as err -> err
         | Ok data ->
           Or_error.try_with (fun () ->
             Crypto.hash_string (hash ()) (Bigstring_shared.to_string data) |> to_hex))
    in
    let md5 = compute_hash ~hash:Hash.md5 in
    let sha256 = compute_hash ~hash:Hash.sha256 in
    let id = { Id.filename; path = Path.to_int_list path } in
    { headers; id; embedded_email; raw_data; md5; sha256 }
  ;;

  let of_content ~headers ~filename ~path content =
    of_content' ~headers ~filename ~path (lazy content)
  ;;

  let of_embedded_email ~headers ~filename ~path embedded_email =
    let content =
      lazy
        (Email.to_bigstring_shared embedded_email
         |> Octet_stream.of_bigstring_shared
              ~encoding:(Octet_stream.Encoding.of_headers_or_default headers))
    in
    of_content' ~embedded_email ~headers ~filename ~path content
  ;;

  let to_file t file =
    match raw_data t with
    | Error _ as err -> Async.return err
    | Ok data -> bigstring_shared_to_file data file
  ;;
end

module Content = struct
  type t = Email.t [@@deriving sexp_of]

  let of_email = ident

  let create_custom
        ~content_type
        ?(encoding = Mimetype.guess_encoding content_type)
        ?(extra_headers = [])
        content
    =
    Expert.content
      ~normalize_headers:`Whitespace
      ~extra_headers:(extra_headers @ [ "Content-Type", content_type ])
      ~encoding
      content
  ;;

  let create = create_custom

  let of_file ?content_type ?encoding ?extra_headers file =
    let open Async in
    let%map content = Reader.file_contents file in
    let content_type =
      match content_type with
      | None -> Mimetype.from_filename file
      | Some content_type -> content_type
    in
    create ~content_type ?encoding ?extra_headers content
  ;;

  let html_utf8 ?(encoding = `Quoted_printable) ?extra_headers content =
    create ?extra_headers ~content_type:Mimetype.html_utf8 ~encoding content
  ;;

  let text_utf8 ?(encoding = `Quoted_printable) ?extra_headers content =
    create ?extra_headers ~content_type:Mimetype.text_utf8 ~encoding content
  ;;

  let create_multipart ?(extra_headers = []) ~content_type = function
    | [] -> failwith "at least one part is required"
    | [ content ] -> add_headers content extra_headers
    | parts ->
      Expert.multipart ~normalize_headers:`Whitespace ~content_type ~extra_headers parts
  ;;

  let alternatives ?extra_headers =
    create_multipart ?extra_headers ~content_type:Mimetype.multipart_alternative
  ;;

  let html_pre str =
    (* This was copy&pasted from [markup.ml] in [html] library
       to avoid adding a dependency on the whole [html] library
       just for this. *)
    let html_encode s =
      let escape =
        [ "&", "&amp;"; "<", "&lt;"; ">", "&gt;"; "\"", "&quot;"; "'", "&#39;" ]
      in
      List.fold ~init:s escape ~f:(fun acc (pattern, with_) ->
        String.substr_replace_all acc ~pattern ~with_)
    in
    (* Gmail decided that text in "pre" elements should wrap by default
       (white-space=pre-wrap), the white-space rule here prevents wrapping and takes
       precendence over their rule. *)
    "<html><pre style=\"white-space: pre !important;\">"
    ^ html_encode str
    ^ "</pre></html>"
  ;;

  let text_monospace_utf8 ?extra_headers content =
    alternatives
      ?extra_headers
      [ text_utf8 ?encoding:None content; html_utf8 ?encoding:None (html_pre content) ]
  ;;

  let mixed ?extra_headers =
    create_multipart ?extra_headers ~content_type:Mimetype.multipart_mixed
  ;;

  let with_related ?(extra_headers = []) ~resources t =
    Expert.multipart
      ~normalize_headers:`Whitespace
      ~content_type:Mimetype.multipart_related
      ~extra_headers
      (add_headers t [ "Content-Disposition", "inline" ]
       :: List.map resources ~f:(fun (name, content) ->
         add_headers content [ "Content-Id", sprintf "<%s>" name ]))
  ;;

  let parse_last_header t name =
    match last_header t name with
    | None -> None
    | Some str ->
      (match String.split str ~on:';' |> List.map ~f:String.strip with
       | [] -> None
       | v :: args ->
         let args =
           List.map args ~f:(fun str ->
             match String.lsplit2 str ~on:'=' with
             | None -> str, None
             | Some (k, v) -> String.strip k, Some (String.strip v))
         in
         Some (v, args))
  ;;

  let content_type t =
    let open Option.Let_syntax in
    parse_last_header t "Content-Type"
    >>| fst
    |> Option.value ~default:"application/x-octet-stream"
  ;;

  let attachment_name t =
    let open Option.Let_syntax in
    let unquote name =
      Option.map name ~f:(fun name ->
        let len = String.length name in
        if len > 2 && name.[0] = '"' && name.[len - 1] = '"'
        then String.sub name ~pos:1 ~len:(len - 2)
        else name)
    in
    Option.first_some
      (let%bind disp, args = parse_last_header t "Content-Disposition" in
       if String.Caseless.equal disp "attachment"
       then
         List.find_map args ~f:(fun (k, v) ->
           if String.Caseless.equal k "filename" then unquote v else None)
       else None)
      (let%bind _, args = parse_last_header t "Content-Type" in
       List.find_map args ~f:(fun (k, v) ->
         if String.Caseless.equal k "name" then unquote v else None))
  ;;

  let related_part_cid t =
    let open Option.Let_syntax in
    let%map str = last_header t "Content-Id" >>| String.strip in
    String.chop_prefix str ~prefix:"<"
    >>= String.chop_suffix ~suffix:">"
    |> Option.value ~default:str
  ;;

  let content_disposition t =
    match parse_last_header t "Content-Disposition" with
    | None -> `Inline
    | Some (disp, _) ->
      if String.Caseless.equal disp "inline"
      then `Inline
      else if String.Caseless.equal disp "attachment"
      then `Attachment (attachment_name t |> Option.value ~default:"unnamed-attachment")
      else (
        match attachment_name t with
        | None -> `Inline
        | Some name -> `Attachment name)
  ;;

  let parts t =
    match Email_content.parse t with
    | Error _ -> None
    | Ok (Email_content.Multipart ts) -> Some ts.Email_content.Multipart.parts
    | Ok (Message _) -> None
    | Ok (Data _) -> None
  ;;

  let content t =
    match Email_content.parse t with
    | Error _ -> None
    | Ok (Email_content.Multipart _) -> None
    | Ok (Message _) -> None
    | Ok (Data data) -> Some data
  ;;

  let rec inline_parts t =
    match parts t with
    | Some parts ->
      if String.Caseless.equal (content_type t) Mimetype.multipart_alternative
      then
        (* multipart/alternative is special since an aplication is expected to
           present/process any one of the alternative parts. The logic for picking
           the 'correct' alternative is application dependant so leaving this to
           to users (e.g. first one that parses) *)
        [ t ]
      else List.concat_map parts ~f:inline_parts
    | None ->
      (match content_disposition t with
       | `Inline -> [ t ]
       | `Attachment _ -> [])
  ;;

  let rec alternative_parts t =
    match parts t with
    | None -> [ t ]
    | Some ts ->
      if String.Caseless.equal (content_type t) Mimetype.multipart_alternative
      then List.concat_map ts ~f:alternative_parts
      else [ t ]
  ;;

  let rec all_related_parts t =
    let get_cid t =
      Option.map (related_part_cid t) ~f:(fun cid -> [ cid, t ])
      |> Option.value ~default:[]
    in
    get_cid t
    @ (parts t |> Option.value ~default:[] |> List.concat_map ~f:all_related_parts)
  ;;

  let find_related t name =
    List.find (all_related_parts t) ~f:(fun (cid, _t) -> String.equal cid name)
    |> Option.map ~f:snd
  ;;

  let to_file t file =
    let open Async in
    match content t with
    | None ->
      Deferred.Or_error.errorf
        "The payload of this email is ambigous, you\n\
        \                  you should decompose the email further"
    | Some content ->
      (match Octet_stream.decode content with
       | None -> Deferred.Or_error.errorf "The message payload used an unknown encoding"
       | Some content -> bigstring_shared_to_file content file)
  ;;

  (* The following are considered deprecated since they leave the charset unspecified
     which has caused issues with some emails not displaying as expected. *)
  let text ?(encoding = `Quoted_printable) ?extra_headers content =
    create ?extra_headers ~content_type:Mimetype.text ~encoding content
  ;;

  let html ?(encoding = `Quoted_printable) ?extra_headers content =
    create ?extra_headers ~content_type:Mimetype.html ~encoding content
  ;;

  let text_monospace ?extra_headers content =
    alternatives
      ?extra_headers
      [ text ?encoding:None content; html ?encoding:None (html_pre content) ]
  ;;
end

type t = Email.t [@@deriving sexp_of]

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
    ?reply_to:(Option.map reply_to ~f:Email_address.to_string)
    ~subject
    ?id
    ?in_reply_to
    ?date:(Option.map date ~f:Email_date.rfc822_date)
    ?auto_generated
    ?extra_headers
    ?attachments
    content
;;

let decode_last_header ?normalize name ~f t =
  Option.bind (last_header ?normalize t name) ~f:(fun v ->
    Option.try_with (fun () -> f v))
;;

let from = decode_last_header "From" ~f:Email_address.of_string_exn
let to_ = decode_last_header "To" ~f:Email_address.list_of_string_exn
let cc = decode_last_header "Cc" ~f:Email_address.list_of_string_exn

let subject =
  decode_last_header ~normalize:`Whitespace_and_encoded_words "Subject" ~f:Fn.id
;;

let id = decode_last_header "Message-Id" ~f:Fn.id

let extract_body ?(content_type = Mimetype.text) email =
  let rec loop email =
    match Email_content.parse email with
    | Error _ -> None
    | Ok (Message _) -> None
    | Ok (Multipart parts) ->
      (* Recursively find the first valid body matching the requested content_type *)
      List.find_map parts.parts ~f:loop
    | Ok (Data stream) ->
      let content_type' = Content.content_type (Content.of_email email) in
      if String.( = ) content_type' content_type
      then (
        match Octet_stream.decode stream with
        | Some decoded -> Some (Bigstring_shared.to_string decoded)
        | None ->
          Async.Log.Global.sexp
            [%message "Failed to decode octet stream" (stream : Octet_stream.t)];
          None)
      else None
  in
  loop email
;;

let all_related_parts = Content.all_related_parts
let find_related = Content.find_related
let inline_parts = Content.inline_parts

let parse_attachment ?container_headers ~path t =
  match Content.content_disposition t with
  | `Inline -> None
  | `Attachment filename ->
    let headers = Email.headers t in
    (match Email_content.parse ?container_headers t with
     | Error _ -> None
     | Ok (Email_content.Multipart _) -> None
     | Ok (Message email) ->
       Some (Attachment.of_embedded_email ~headers ~filename ~path email)
     | Ok (Data content) -> Some (Attachment.of_content ~headers ~filename ~path content))
;;

let map_file_attachments t ~f =
  let handle_possible_attachment ?container_headers ~path t =
    parse_attachment ?container_headers ~path t
    |> function
    | None -> `Unchanged
    | Some attachment ->
      (match f attachment with
       | `Keep -> `Unchanged
       | `Replace attachment' -> `Changed attachment')
  in
  let rec loop ?container_headers t ~path =
    match Email_content.parse ?container_headers t with
    | Error _ -> `Unchanged
    | Ok (Data _data) -> handle_possible_attachment ?container_headers ~path t
    | Ok (Message message) ->
      (match loop message ?container_headers:None ~path:(Path.child path 0) with
       | `Unchanged -> `Unchanged
       | `Changed message' -> `Changed (Email_content.set_content t (Message message')))
    | Ok (Multipart (mp : Email_content.Multipart.t)) ->
      (match
         List.fold_mapi mp.parts ~init:`Unchanged ~f:(fun i change_status t ->
           match
             loop ~container_headers:mp.container_headers ~path:(Path.child path i) t
           with
           | `Unchanged -> change_status, t
           | `Changed t -> `Changed, t)
       with
       | `Unchanged, _ -> `Unchanged
       | `Changed, parts' ->
         let mp' = Email_content.Multipart.set mp ~parts:parts' () in
         `Changed (Email_content.set_content t (Multipart mp')))
  in
  match loop ?container_headers:None ~path:Path.root t with
  | `Unchanged -> t
  | `Changed t -> t
;;

let all_attachments t =
  let all_attachments = ref [] in
  let handle_possible_attachment ?container_headers ~path t =
    parse_attachment ?container_headers ~path t
    |> Option.iter ~f:(fun attachment ->
      all_attachments := attachment :: !all_attachments)
  in
  let rec loop ?container_headers t ~path =
    match Email_content.parse ?container_headers t with
    | Error _ -> ()
    | Ok (Data _data) -> handle_possible_attachment ?container_headers ~path t
    | Ok (Message message) ->
      handle_possible_attachment ?container_headers ~path t;
      loop message ?container_headers:None ~path:(Path.child path 0)
    | Ok (Multipart (mp : Email_content.Multipart.t)) ->
      List.iteri mp.parts ~f:(fun i t ->
        loop ~container_headers:mp.container_headers ~path:(Path.child path i) t)
  in
  loop ?container_headers:None ~path:Path.root t;
  List.rev !all_attachments
;;

let find_attachment t name =
  List.find (all_attachments t) ~f:(fun attachment ->
    String.equal (Attachment.filename attachment) name)
;;
