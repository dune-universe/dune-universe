(* https://www.dropbox.com/developers/core/docs *)

open Printf
open Lwt
module Json = Dropbox_j
module Date = Dropbox_date

(* Error handling
 ***********************************************************************)

type error_description = Json.error_description
                       = { error: string;
                           error_description: string }

type error =
  | Invalid_arg of error_description
  | Invalid_token of error_description
  | Invalid_oauth of error_description
  | Conflict of error_description
  | Too_many_requests of error_description
  | Try_later of int option * error_description
  | Quota_exceeded of error_description
  | Server_error of int * error_description
  | Not_modified of error_description
  | Unsupported_media_type of error_description

(* FIXME: Do we want to render the values as strings closer to OCaml? *)
let string_of_error = function
  | Invalid_arg e ->
     "Invalid_arg " ^ Json.string_of_error_description e
  | Invalid_token e ->
     "Invalid_token " ^ Json.string_of_error_description e
  | Invalid_oauth e ->
     "Invalid_oauth " ^ Json.string_of_error_description e
  | Conflict e ->
     "Conflict " ^ Json.string_of_error_description e
  | Too_many_requests e ->
     "Too_many_requests " ^ Json.string_of_error_description e
  | Try_later (sec, e) ->
     (match sec with
      | None -> "Try_later " ^ Json.string_of_error_description e
      | Some s ->
         sprintf "Try_later(%i, %s)" s (Json.string_of_error_description e))
  | Quota_exceeded e ->
     "Quota_exceeded " ^ Json.string_of_error_description e
  | Server_error (st, e) ->
     sprintf "Server_error(%i, %s)" st (Json.string_of_error_description e)
  | Not_modified e ->
     "Not_modified " ^ Json.string_of_error_description e
  | Unsupported_media_type e ->
     "Unsupported_media_type " ^ Json.string_of_error_description e

exception Error of error

let () =
  Printexc.register_printer (function Error e -> Some(string_of_error e)
                                    | _exn -> None)


let fail_error body f =
  Cohttp_lwt.Body.to_string body >>= fun body ->
  let e = Json.error_description_of_string body in
  fail(Error(f e))

let check_errors_k k ((rq, body) as r) =
  match rq.Cohttp.Response.status with
  | `Bad_request -> fail_error body (fun e -> Invalid_arg e)
  | `Unauthorized -> fail_error body (fun e -> Invalid_token e)
  | `Forbidden -> fail_error body (fun e -> Invalid_oauth e)
  | `Conflict -> fail_error body (fun e -> Conflict e)
  | `Too_many_requests -> fail_error body (fun e -> Too_many_requests e)
  | `Service_unavailable ->
     (match Cohttp.(Header.get rq.Response.headers "retry-after") with
      | None -> fail_error body (fun e -> Try_later(None, e))
      | Some retry ->
         try
           (* FIXME: [retry] can also be a date *)
           let s = int_of_string retry in
           fail_error body (fun e -> Try_later(Some s, e))
         with _ ->
           fail_error body (fun e -> Try_later(None, e)) )
  | `Insufficient_storage -> fail_error body (fun e -> Quota_exceeded e)
  | `Not_modified -> fail_error body (fun e -> Not_modified e)
  | `Unsupported_media_type -> fail_error body
                               (fun e -> Unsupported_media_type e)
  | _ -> k r

let check_errors =
  let std_err ((rq, body) as r) =
    let st = Cohttp.Code.code_of_status rq.Cohttp.Response.status  in
    if Cohttp.Code.is_error st then
      fail_error body (fun e -> Server_error(st, e))
    else
       return(r) in
  check_errors_k std_err

let check_errors_404 f =
  let std_err ((rq, body) as r) =
    if rq.Cohttp.Response.status = `Not_found then
      return None
    else
      let st = Cohttp.Code.code_of_status rq.Cohttp.Response.status in
      if Cohttp.Code.is_error st then
        fail_error body (fun e -> Server_error(st, e))
      else
        f r in
  check_errors_k std_err


(* Signature & Functor
 ***********************************************************************)

module type S = sig

  module OAuth2 : sig
    val authorize : ?state: string ->
                    ?force_reapprove: bool ->
                    ?disable_signup: bool ->
                    id: string ->
                    [`Token of Uri.t | `Code of Uri.t option] -> Uri.t

    type code = string
    val code_of_uri : Uri.t -> (code * string) option

    type token = string

    val token_of_uri : Uri.t -> (token * string) option

    val token : ?redirect_uri: Uri.t ->
                code -> id: string -> secret: string -> token Lwt.t
  end

  type t
  val session : OAuth2.token -> t
  val token : t -> OAuth2.token

  type name_details = Dropbox_t.name_details
                    = { familiar_name: string;
                        given_name: string;
                        surname: string }

  type team = Dropbox_t.team
            = { name: string;
                team_id: int }

  type quota_info = Dropbox_t.quota_info
                  = { shared: int;
                      quota: int;
                      normal: int }

  type info = Dropbox_t.info
            = { uid: int;
                display_name: string;
                email_verified: bool;
                name_details: name_details;
                referral_link: Uri.t;
                country: string;
                locale: string;
                is_paired: bool;
                team: team option;
                quota_info: quota_info }

  val info : ?locale: string -> t -> info Lwt.t

  type photo_info = Dropbox_json.Photo.info
                  = { time_taken: Date.t option;
                      lat_long: (float * float) option }

  type video_info = Dropbox_json.Video.info
                  = { time_taken: Date.t option;
                      duration: float option;
                      lat_long: (float * float) option }

  type user = Dropbox_t.user
            = { uid: int;
                display_name: string;
                same_team: bool;
                member_id: string }

  type user_info = Dropbox_t.user_info
                 = { user: user;
                     access_type: string;
                     active: bool }

  type group = Dropbox_t.group
             = { group_name: string;
                 group_id: string;
                 num_members: int }

  type shared_folder = Dropbox_t.shared_folder
                     = { shared_folder_id: string;
                         shared_folder_name: string;
                         path: string;
                         access_type: string;
                         shared_link_policy: string;
                         owner: user option;
                         membership: user_info list;
                         groups: group list }

  type metadata = Dropbox_t.metadata = {
      size: string;
      bytes: int;
      mime_type: string;
      path: string;
      is_dir: bool;
      is_deleted: bool;
      rev: string;
      hash: string;
      thumb_exists: bool;
      photo_info: [ `None | `Pending | `Some of photo_info ];
      video_info: [ `None | `Pending | `Some of video_info ];
      icon: string;
      modified: Date.t option;
      client_mtime: Date.t option;
      root: [ `Dropbox | `App_folder ];
      contents: metadata list;
      shared_folder: shared_folder option;
      read_only: bool;
      parent_shared_folder_id: int;
      modifier: user option }

  type cursor

  (* It is better that [delta] is not shared between various
     instantiations of the functor so it is good it does not appear as
     an alias. *)
  type delta
    = { entries: (string * metadata option) list;
        reset: bool;
        cursor: cursor;
        has_more: bool }

  type longpoll_delta
    = Dropbox_t.longpoll_delta
    = { changes: bool;
        backoff: int option }

  type copy_ref
    = Dropbox_t.copy_ref
    = { copy_ref: string;
        expires: Date.t }

  type link
    = Dropbox_t.link
    = { url: string;
        expires: Date.t }

  type visibility = [
    | `Public
    | `Team_only
    | `Password
    | `Team_and_password
    | `Shared_folder_only
    | `Other of string
    ]

  type shared_link
    = Dropbox_t.shared_link
    = { url: string;
        expires: Date.t;
        visibility: visibility }

  type chunked_upload_id = private string

  type chunked_upload
    = { id: chunked_upload_id;
        ofs: int;
        expires: Date.t }

  val get_file : t -> ?rev: string -> ?start: int -> ?len: int ->
                 string -> (metadata * string Lwt_stream.t) option Lwt.t

  val metadata : t -> ?file_limit: int -> ?hash: string -> ?list: bool ->
                 ?include_deleted: bool -> ?rev: string -> ?locale: string ->
                 ?include_media_info: bool -> ?include_membership: bool ->
                 string -> metadata option Lwt.t

  val delta : ?cursor: cursor -> ?locale: string -> ?path_prefix: string
              -> ?include_media_info: bool -> t -> delta Lwt.t

  val latest_cursor : ?path_prefix: string -> ?include_media_info: bool
                      -> t -> cursor Lwt.t

  val longpoll_delta : t -> ?timeout: int -> cursor -> longpoll_delta Lwt.t

  val revisions : t -> ?rev_limit: int -> ?locale: string -> string ->
                  metadata list option Lwt.t

  val restore : t -> ?locale: string -> rev:string -> string ->
                metadata option Lwt.t

  val search : t -> ?file_limit: int -> ?include_deleted: bool ->
               ?locale: string -> ?include_membership: bool ->
               ?fn: string -> string -> metadata list Lwt.t

  val copy_ref : t -> string -> copy_ref option Lwt.t

  val shares : t -> ?locale: string -> ?short_url: bool ->
               string -> shared_link option Lwt.t

  val media : t -> ?locale: string -> string -> link option Lwt.t


  val shared_folders : ?shared_folder_id: string -> ?include_membership: bool
                      -> t -> shared_folder list Lwt.t

  val files_put :
    t -> ?locale: string -> ?overwrite: bool ->
    ?parent_rev: string -> ?autorename: bool -> string ->
    [ `String of string
    | `Strings of string list
    | `Stream of string Lwt_stream.t
    | `Stream_len of string Lwt_stream.t * int] -> metadata Lwt.t

  val chunked_upload :
    t -> ?id: chunked_upload_id -> ?ofs: int ->
    [ `String of string
    | `Strings of string list
    | `Stream of string Lwt_stream.t ] -> chunked_upload Lwt.t

  val commit_chunked_upload : t -> ?locale: string -> ?overwrite: bool ->
                              ?parent_rev: string -> ?autorename: bool ->
                              chunked_upload_id -> string -> metadata Lwt.t

  val thumbnails : t -> ?format: [ `Jpeg | `Png  | `Bmp ]
                   -> ?size: [ `Xs | `S | `M | `L | `Xl ] -> string ->
                   (metadata * string Lwt_stream.t) option Lwt.t

  val previews : t -> ?rev: string -> string ->
                 (string * string * string Lwt_stream.t) option Lwt.t


  type root = [ `Auto | `Dropbox | `Sandbox ]

  val copy : t -> ?locale: string -> ?root: root ->
             [ `From_path of string | `From_copy_ref of string ] ->
             string -> [ `Some of metadata
                       | `None | `Invalid of string
                       | `Too_many_files ] Lwt.t

  val create_folder : t -> ?locale: string -> ?root: root ->
                      string -> [ `Some of metadata
                                   | `Invalid of string ] Lwt.t

  val delete :
    t -> ?locale: string -> ?root: root ->
    string -> [ `Some of metadata | `None | `Too_many_files ] Lwt.t

  val move : t -> ?locale: string -> ?root: root ->
             string -> string -> [ `Some of metadata
                                 | `None
                                 | `Invalid of string
                                 | `Too_many_files ] Lwt.t
end

module Make(Client: Cohttp_lwt.S.Client) = struct

  module OAuth2 = struct

    let authorize_uri =
      Uri.of_string "https://www.dropbox.com/1/oauth2/authorize"

    let authorize ?(state="") ?(force_reapprove=false) ?(disable_signup=false)
                  ~id:client_id response =
      let q = ["client_id", [client_id];
               "state", [state];
               "force_reapprove", [string_of_bool force_reapprove];
               "disable_signup", [string_of_bool disable_signup] ] in
      let q = match response with
        | `Token uri -> ("response_type", ["token"])
                       :: ("redirect_uri", [Uri.to_string uri]) :: q
        | `Code(Some uri) -> ("response_type", ["code"])
                            :: ("redirect_uri", [Uri.to_string uri]) :: q
        | `Code None -> ("response_type", ["code"]) :: q in
      Uri.with_query authorize_uri q

    type code = string

    let code_of_uri u =
      match Uri.get_query_param u "code" with
      | Some code ->
         let state = match Uri.get_query_param u "state" with
           | Some s -> s
           | None -> "" in
         Some(code, state)
      | None -> None

    type token = string

    let get_query_param name q = String.concat "" (List.assoc name q)

    let token_of_uri u =
      match Uri.fragment u with
      | None -> None
      | Some f ->
         let q = Uri.query_of_encoded f in
         try
           let state = try get_query_param "state" q
                       with _ -> "" in
           Some (get_query_param "access_token" q,  state)
         with Not_found -> None

    let token_uri = Uri.of_string "https://api.dropbox.com/1/oauth2/token"

    let token ?redirect_uri code ~id ~secret =
      (* FIXME: do we want to allow the possibility to use HTTP basic
       authentication (and why/why not)? *)
      let q = ["code", [code];
               "grant_type", ["authorization_code"];
               "client_id", [id];
               "client_secret", [secret] ] in
      let q = match redirect_uri with
        | None -> q
        | Some u -> ("redirect_uri", [Uri.to_string u]) :: q in
      Client.post (Uri.with_query token_uri q) >>=
      check_errors >>= fun (_, body) ->
      Cohttp_lwt.Body.to_string body >>= fun body ->
      return((Json.token_of_string body).Json.access_token)
  end

  include Dropbox_t

  type photo_info = Dropbox_json.Photo.info
                  = { time_taken: Date.t option;
                      lat_long: (float * float) option }

  type video_info = Dropbox_json.Video.info
                  = { time_taken: Date.t option;
                      duration: float option;
                      lat_long: (float * float) option }

  type chunked_upload_id = string

  type t = OAuth2.token

  let token t = t

  let session token = token

  let headers (t: t) =
    let bearer = "Bearer " ^ (token t) in
    Cohttp.Header.init_with "Authorization" bearer

  let info_uri = Uri.of_string "https://api.dropbox.com/1/account/info"

  let info ?locale t =
    let u = match locale with
      | None -> info_uri
      | Some l -> Uri.with_query info_uri ["locale", [l]] in
    Client.get ~headers:(headers t) u >>= check_errors >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body >>= fun body ->
    return(Json.info_of_string body)


  let get_metadata k (r, body) =
    (* Extract content metadata from the header *)
    match Cohttp.(Header.get r.Response.headers "x-dropbox-metadata") with
    | Some h ->
       let metadata = Json.metadata_of_string h in
       k metadata body
    | None ->
       (* Should not happen *)
       let msg = {
           error = "x-dropbox-metadata";
           error_description = "Missing x-dropbox-metadata header" } in
       fail(Error(Server_error(500, msg)))

  let stream_of_file =
    get_metadata (fun metadata body ->
                  return(Some(metadata, Cohttp_lwt.Body.to_stream body)))

  let empty_stream =
    get_metadata (fun metadata body ->
                  Cohttp_lwt.Body.drain_body body >>= fun () ->
                  return(Some(metadata, Lwt_stream.of_list [])))

  let get_file t ?rev ?start ?len fn =
    let headers = headers t in
    let headers, must_download = match start, len with
      | Some s, Some l ->
         let s = if s < 0 then 0 else s in
         if l <= 0 then (Cohttp.Header.add headers "Range" ("bytes=0-0"), false)
         else
           let range = string_of_int s ^ "-" ^ string_of_int(s + l - 1) in
           (Cohttp.Header.add headers "Range" ("bytes=" ^ range),
            true)
      | Some s, None ->
         let range = string_of_int s ^ "-" in
         (Cohttp.Header.add headers "Range" ("bytes=" ^ range),
          true)
      | None, Some l ->
         if l <= 0 then (Cohttp.Header.add headers "Range" ("bytes=0-0"), false)
         else
           (Cohttp.Header.add headers "Range" ("bytes=-" ^ string_of_int l),
            true)
      | None, None -> headers, true in
    let u =
      Uri.of_string("https://api-content.dropbox.com/1/files/auto/" ^ fn) in
    let u = match rev with None -> u
                         | Some r -> Uri.with_query u ["rev", [r]] in
    Client.get ~headers u
    >>= check_errors_404 (if must_download then stream_of_file
                          else empty_stream)

  let metadata_of_response (_, body) =
    Cohttp_lwt.Body.to_string body
    >>= fun body -> return(Some(Json.metadata_of_string body))

  let metadata t ?(file_limit=10_000) ?(hash="") ?(list=true)
               ?(include_deleted=false) ?(rev="") ?(locale="")
               ?(include_media_info=false) ?(include_membership=true) fn =
    let u = Uri.of_string("https://api.dropbox.com/1/metadata/auto/" ^ fn) in
    let file_limit = if file_limit < 0 then 0 else file_limit in
    let q = [("list", [string_of_bool list]);
             ("file_limit", [string_of_int file_limit]);
             ("include_deleted", [string_of_bool include_deleted]);
             ("include_media_info", [string_of_bool include_media_info]);
             ("include_membership", [string_of_bool include_membership])
            ] in
    let q = if hash <> "" then ("hash", [hash]) :: q else q in
    let q = if locale <> "" then ("locale", [locale]) :: q else q in
    let q = if rev <> "" then ("rev", [rev]) :: q else q in
    let u = Uri.with_query u q in
    Client.get ~headers:(headers t) u
    >>= check_errors_404 metadata_of_response

  type cursor = { cursor: string;
                  path_prefix: string;
                  include_media_info: bool }

  type delta = { entries: (string * Json.metadata option) list;
                 reset: bool;
                 cursor: cursor;
                 has_more: bool }

  (* FIXME: [path_concat] should probably be in Uri. *)
  let path_concat p1 p2 =
    if p2 = "" then p1
    else if p1 = "" then p2
    else if p1.[String.length p1 - 1] = '/' then p1 ^ p2
    else p1 ^ "/" ^ p2

  let delta_uri = Uri.of_string "https://api.dropbox.com/1/delta"

  let delta ?cursor ?(locale="") ?(path_prefix="")
            ?(include_media_info=false) t =
    let param = match cursor with
      | Some (c: cursor) ->
         let param = [("cursor", [c.cursor])] in
         let param =
           if c.path_prefix <> "" then
             ("path_prefix", [path_concat c.path_prefix path_prefix]) :: param
           else param in
         if c.include_media_info then
           ("include_media_info",[string_of_bool c.include_media_info])
           :: param
         else param
      | None ->
         let param =
           [("include_media_info", [string_of_bool include_media_info])] in
         if path_prefix <> "" then ("path_prefix", [path_prefix]) :: param
         else param in
    let param = if locale <> "" then ("locale", [locale]) :: param
                else param in
    let u = Uri.with_query delta_uri param in
    Client.post ~headers:(headers t) u
    >>= check_errors >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body >>= fun body ->
    let delta = Json.delta_json_of_string body in
    let cursor = { cursor = delta.Json.cursor;
                   path_prefix;
                   include_media_info } in
    return({ entries = delta.Json.entries;
             reset = delta.Json.reset;
             cursor;
             has_more = delta.Json.has_more })


  let latest_cursor_uri =
    Uri.of_string "https://api.dropbox.com/1/delta/latest_cursor"

  let latest_cursor ?(path_prefix="") ?(include_media_info=false) t =
    let param = [("include_media_info",[string_of_bool include_media_info])] in
    let param =
      if path_prefix <> "" then ("path_prefix", [path_prefix]) :: param
      else param in
    let u = Uri.with_query latest_cursor_uri param in
    Client.post ~headers:(headers t) u
    >>= check_errors >>= fun(_, body) ->
    Cohttp_lwt.Body.to_string body >>= fun body ->
    let c = Json.latest_cursor_of_string body in
    return({ cursor = c.Json.latest_cursor;
             path_prefix;
             include_media_info })


  let longpoll_delta_uri =
    Uri.of_string "https://api-notify.dropbox.com/1/longpoll_delta"

  let longpoll_delta t ?(timeout=30) (c: cursor) =
    let timeout = if timeout < 30 then 30
                  else if timeout > 480 then 480
                  else timeout in
    let param = [("timeout", [string_of_int timeout]);
                 ("cursor", [c.cursor])] in
    let u = Uri.with_query longpoll_delta_uri param in
    Client.get ~headers:(headers t) u >>= check_errors
    >>= fun(_, body) -> Cohttp_lwt.Body.to_string body
    >>= fun body -> return(Json.longpoll_delta_of_string body)

  let metadata_list_of_response (_, body) =
    Cohttp_lwt.Body.to_string body
    >>= fun body -> return(Some(Json.metadata_list_of_string body))

  let revisions t ?(rev_limit=10) ?(locale="") fn =
    let u = Uri.of_string("https://api.dropbox.com/1/revisions/auto/" ^ fn) in
    let rev_limit = if rev_limit < 0 then 0
                    else if rev_limit > 1000 then 1000
                    else rev_limit in
    let q = [("rev_limit",[string_of_int rev_limit])] in
    let q = if locale <> "" then ("locale",[locale]) :: q else q in
    let u = Uri.with_query u q in
    Client.get ~headers:(headers t) u
    >>= check_errors_404 metadata_list_of_response

 let restore t ?(locale="") ~rev fn =
    let u = Uri.of_string("https://api.dropbox.com/1/restore/auto/" ^ fn) in
    let q = [("rev",[rev])] in
    let q = if locale <> "" then ("locale",[locale]) :: q else q in
    let u = Uri.with_query u q in
    Client.post ~headers:(headers t) u
    >>= check_errors_404 metadata_of_response

  let search t ?(file_limit=1000) ?(include_deleted=false)
             ?(locale="") ?(include_membership=false) ?(fn="") query =
    let u = if fn <> "" then
              Uri.of_string("https://api.dropbox.com/1/search/auto/" ^ fn)
            else Uri.of_string("https://api.dropbox.com/1/search/auto/") in
    let file_limit = if file_limit < 0 then 0
                     else if file_limit > 1000 then 1000
                     else file_limit in
    let q = [("include_deleted",[string_of_bool include_deleted]);
             ("include_membership",[string_of_bool include_membership]);
             ("file_limit", [string_of_int file_limit]);
             ("query", [query]) ] in
    let q = if locale <> "" then ("locale",[locale]) :: q else q in
    let u = Uri.with_query u q in
    Client.get ~headers:(headers t) u >>= check_errors
    >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
    >>= fun body -> return(Json.metadata_list_of_string body)

  let copy_ref_of_response (_, body) =
    Cohttp_lwt.Body.to_string body
    >>= fun body -> return(Some(Json.copy_ref_of_string body))

  let copy_ref t fn =
    let u = Uri.of_string("https://api.dropbox.com/1/copy_ref/auto/" ^ fn) in
    Client.get ~headers:(headers t) u
    >>= check_errors_404 copy_ref_of_response

  let shares_of_response (_, body) =
    Cohttp_lwt.Body.to_string body
    >>= fun body -> return(Some(Json.shared_link_of_string body))

  let shares t ?(locale="") ?(short_url=true) fn =
    let u = Uri.of_string("https://api.dropbox.com/1/shares/auto/" ^ fn) in
    let q = [("short_url",[string_of_bool short_url])] in
    let q = if locale <> "" then ("locale",[locale]) :: q else q in
    let u = Uri.with_query u q in
    Client.post ~headers:(headers t) u
    >>= check_errors_404 shares_of_response

  let media_of_response (_, body) =
    Cohttp_lwt.Body.to_string body
    >>= fun body -> return(Some(Json.link_of_string body))

  let media t ?(locale="") fn =
    let u = Uri.of_string("https://api.dropbox.com/1/media/auto/" ^ fn) in
    let u = if locale <> "" then Uri.with_query u [("local",[locale])] else u in
    Client.post ~headers:(headers t) u
    >>= check_errors_404 media_of_response

  let shared_folders_uri =
    Uri.of_string "https://api.dropbox.com/1/shared_folders/"

  let shared_folders ?(shared_folder_id="") ?(include_membership=true) t =
    let u = if shared_folder_id <> "" then
              Uri.of_string("https://api.dropbox.com/1/shared_folders/"
                            ^ shared_folder_id)
            else shared_folders_uri in
    let q = ["include_membership", [string_of_bool include_membership]] in
    Client.get ~headers:(headers t) (Uri.with_query u q)
    >>= check_errors
    >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
    >>= fun body -> match shared_folder_id with
    | "" -> return(Json.shared_folders_of_string body)
    | _ -> return [Json.shared_folder_of_string body]


  let add_content_length headers len =
    Cohttp.Header.add headers "Content-Length" (string_of_int len)

  let files_put t ?(locale="") ?(overwrite=true) ?(parent_rev="")
                ?(autorename=true) fn content =
    let headers = headers t in
    let u =
      Uri.of_string("https://api-content.dropbox.com/1/files_put/auto/" ^ fn) in
    let q = [("overwrite", [string_of_bool overwrite]);
             ("autorename", [string_of_bool autorename])] in
    let q = if locale <> "" then ("locale", [locale]) :: q else q in
    let q = if parent_rev <> "" then ("parent_rev",[parent_rev]) :: q else q in
    let headers, body = match content with
      | `String s as b ->
         add_content_length headers (String.length s), b
      | `Strings l as b ->
         let len = List.fold_left (fun l s -> l + String.length s) 0 l in
         add_content_length headers len, b
      | `Stream stream ->
         (* Although Content-Length is required, Dropbox seems to do
            fine without (except to verify that all bytes have been
            transmitted of course). *)
         headers, Cohttp_lwt.Body.of_stream stream
      | `Stream_len (stream, len) ->
         (add_content_length headers len,
          Cohttp_lwt.Body.of_stream stream) in
    Client.put ~headers ~body (Uri.with_query u q)
    >>= check_errors >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
    >>= fun body -> return(Json.metadata_of_string body)

  let chunked_upload_uri =
    Uri.of_string "https://api-content.dropbox.com/1/chunked_upload"

  let chunked_upload t ?(id="") ?(ofs=0) chunked_data =
    let q = if id <> "" then [("upload_id", [id])] else [] in
    let q = if ofs <> 0 then ("offset", [string_of_int ofs]) :: q else q in
    let u = Uri.with_query chunked_upload_uri q in
    Client.put ~body:(chunked_data :> Cohttp_lwt.Body.t)
               ~chunked:true ~headers:(headers t) u
    >>= check_errors >>= fun (_, body) ->
    Cohttp_lwt.Body.to_string body >>= fun body ->
    return(Json.chunked_upload_of_string body)


  let commit_chunked_upload t ?(locale="") ?(overwrite=true) ?(parent_rev="")
                            ?(autorename=true) upload_id fn =
    let u = Uri.of_string("https://api-content.dropbox.com/1\
                           /commit_chunked_upload/auto/" ^ fn) in
    let q = [("overwrite",[string_of_bool overwrite]);
             ("autorename",[string_of_bool autorename]);
             ("upload_id",[upload_id])] in
    let q = if locale <> "" then ("locale", [locale]) :: q else q in
    let q = if parent_rev <> "" then ("parent_rev",[parent_rev]) :: q else q in
    let u = Uri.with_query u q in
    Client.post ~headers:(headers t) u >>= check_errors
    >>= fun (_, body) -> Cohttp_lwt.Body.to_string body
    >>= fun body -> return(Json.metadata_of_string body)


  let thumbnails t ?(format=`Jpeg) ?(size=`S) fn =
    let u = Uri.of_string
              ("https://api-content.dropbox.com/1/thumbnails/auto/" ^ fn) in
    let q = ["size", [match size with
                      | `Xs -> "xs"
                      | `S -> "s"
                      | `M -> "m"
                      | `L -> "l"
                      | `Xl -> "xl"] ] in
    let q = ("format", [match format with
                        | `Jpeg -> "jpeg"
                        | `Png ->  "png"
                        | `Bmp ->  "bmp"]) :: q in
    Client.get ~headers:(headers t) (Uri.with_query u q)
    >>= check_errors_404 stream_of_file


  let stream_of_file_prev (r, body) =
    (* Extract content-type and original-content-length from the header *)
    let open Cohttp in
    match Header.get r.Response.headers "Content-Type",
          Header.get r.Response.headers "Original-Content-Length" with
    | Some content_type, Some content_length ->
       let body_stream = Cohttp_lwt.Body.to_stream body in
       return(Some(content_type, content_length, body_stream))
    | _ , _ ->
       (* Should not happen *)
       let msg = {
           error = "content-length/type";
           error_description = "Missing content-length or content-type header"
         } in
       fail(Error(Server_error(500, msg)))

  let previews t ?(rev="") fn =
    let u =
      Uri.of_string("https://api-content.dropbox.com/1/previews/auto/" ^ fn) in
    let u = if rev <> "" then Uri.with_query u ["rev",[rev]] else u in
    Client.get ~headers:(headers t) u
    >>= check_errors_404 stream_of_file_prev


  (* File operations *)

  type root = [ `Auto | `Dropbox | `Sandbox ]

  let copy_uri =
    Uri.of_string("https://api.dropbox.com/1/fileops/copy")

  let is_OAuth_substring s =
    try
      let i = String.index s 'O' in
      i + 4 < String.length s
      && s.[i+1] = 'A' && s.[i+2] = 'u' && s.[i+3] = 't' && s.[i+4] = 'h'
    with Not_found -> false

  let error_oauth_or_invalid body =
    Cohttp_lwt.Body.to_string body >>= fun body ->
    let e = Json.error_description_of_string body in
    if is_OAuth_substring e.error then
      fail(Error(Invalid_oauth e))
    else
      return(`Invalid e.error)

  let metadata_of_response (_, body) =
    Cohttp_lwt.Body.to_string body >>= fun body ->
    return(`Some(Json.metadata_of_string body))

  let copy_response ((rq, body) as r) =
    (* Fobidden is used when the copy operation is not allowed (in
       addition to incorrect OAuth).  Try to distinguish the two to
       have finer error reporting. *)
    match rq.Cohttp.Response.status with
    | `Not_found -> return `None
    | `Forbidden -> error_oauth_or_invalid body
    | `Not_acceptable -> return `Too_many_files
    | _ -> check_errors r >>= metadata_of_response

  let copy t ?(locale="") ?(root=`Auto) source to_path =
    let q = [("to_path",[to_path])] in
    let q = if locale <> "" then ("locale",[locale]) :: q else q in
    let q = match source with
      | `From_path from_path -> ("from_path",[from_path]) :: q
      | `From_copy_ref copy_ref -> ("from_copy_ref",[copy_ref]) :: q in
    let q = ("root", [match root with
                      | `Auto -> "auto"
                      | `Dropbox -> "dropbox"
                      | `Sandbox -> "sandbox"]) :: q in
    let u = Uri.with_query copy_uri q in
    Client.post ~headers:(headers t) u
    >>= copy_response

  let create_folder_uri =
    Uri.of_string("https://api.dropbox.com/1/fileops/create_folder")

  let create_folder_response ((rq, body) as r) =
    match rq.Cohttp.Response.status with
    | `Forbidden -> error_oauth_or_invalid body
    | _ -> check_errors r >>= metadata_of_response

  let create_folder t ?(locale="") ?(root=`Auto) path =
    let q = [("path",[path])] in
    let q = if locale <> "" then ("locale",[locale]) :: q else q in
    let q = match root with
      | `Auto -> ("root",["auto"]) :: q
      | `Dropbox -> ("root",["dropbox"]) :: q
      | `Sandbox -> ("root",["sandbox"]) :: q in
    let u = Uri.with_query create_folder_uri q in
    Client.post ~headers:(headers t) u
    >>= create_folder_response

  let delete_uri =
    Uri.of_string("https://api.dropbox.com/1/fileops/delete")

  let delete_response ((rq, _) as r) =
    match rq.Cohttp.Response.status with
    | `Not_found -> return `None
    | `Not_acceptable -> return `Too_many_files
    | _ -> check_errors r >>= metadata_of_response

  let delete t ?(locale="") ?(root=`Auto) path =
    let q = [("path",[path])] in
    let q = if locale <> "" then ("locale",[locale]) :: q else q in
    let q = match root with
      | `Auto -> ("root",["auto"]) :: q
      | `Dropbox -> ("root",["dropbox"]) :: q
      | `Sandbox -> ("root",["sandbox"]) :: q in
    let u = Uri.with_query delete_uri q in
    Client.post ~headers:(headers t) u
    >>= delete_response

  let move_uri = Uri.of_string("https://api.dropbox.com/1/fileops/move")

  let move_response ((rq, body) as r) =
    match rq.Cohttp.Response.status with
    | `Forbidden -> error_oauth_or_invalid body
    | `Not_found -> return `None
    | `Not_acceptable -> return `Too_many_files
    | _ -> check_errors r >>= metadata_of_response

  let move t ?(locale="") ?(root=`Auto) from_path to_path =
    let q = [("from_path",[from_path]);("to_path",[to_path])] in
    let q = if locale <> "" then ("locale",[locale]) :: q else q in
    let q = match root with
      | `Auto -> ("root",["auto"]) :: q
      | `Dropbox -> ("root",["dropbox"]) :: q
      | `Sandbox -> ("root",["sandbox"]) :: q in
    let u = Uri.with_query move_uri q in
    Client.post ~headers:(headers t) u
    >>= move_response
end
