(** The base URL of the software heritage instance used, defaults to
    [https://archive.softwareheritage.org]. *)
let instance = ref "https://archive.softwareheritage.org"

(**/**)

let url endpoint = Format.sprintf "%s/api/1%s" !instance endpoint

let field_not_found f =
  Error (Format.sprintf "field `%s` not found in the JSON response" f)

let on_response url f =
  match Ezcurl.get ~url () with
  | Error (code, msg) ->
    Error (Format.sprintf "curl error: code `%s` (%s)" (Curl.strerror code) msg)
  | Ok response -> (
    match Json.json_of_src (`String response.body) with
    | Error (_loc, _e) ->
      Error (Format.sprintf "error while parsing JSON response")
    | Ok response -> f response )

(**/**)

(** Same as [content] but expects an object identifier hash directly. *)
let content_unsafe ~hash_type (hash : Lang.object_id) =
  let url = url (Format.sprintf "/content/%s:%s/" hash_type hash) in
  on_response url (fun response ->
      let field = "data_url" in
      match Json.find_string field response with
      | Some data_url -> Ok data_url
      | None -> field_not_found field )

(** For a given content identifier, compute an URL from which the content can be
    downloaded. *)
let content id =
  match Lang.get_object_type id with
  | Content hash_type -> content_unsafe ~hash_type @@ Lang.get_object_id id
  | Directory
  | Release
  | Revision
  | Snapshot ->
    Error "invalid object type (expected Content)"

(** Same as [directory] but expects an object identifier hash directly. *)
let directory_unsafe (hash : Lang.object_id) =
  let url = url (Format.sprintf "/vault/directory/%s/" hash) in
  match Ezcurl.post ~params:[] ~url () with
  | Error (code, msg) ->
    Error (Format.sprintf "curl error: code `%s` (%s)" (Curl.strerror code) msg)
  | Ok response -> (
    match Json.json_of_src (`String response.body) with
    | Error (_loc, _e) ->
      Error (Format.sprintf "error while parsing JSON response")
    | Ok _response ->
      on_response url (fun response ->
          let field = "fetch_url" in
          match Json.find_string field response with
          | Some data_url -> Ok data_url
          | None -> field_not_found field ) )

(** For a given directory identifier, compute an URL from which the directory
    can be downloaded. *)
let directory id =
  match Lang.get_object_type id with
  | Directory -> directory_unsafe @@ Lang.get_object_id id
  | Content _
  | Release
  | Revision
  | Snapshot ->
    Error "invalid object type (expected Directory)"

(** Same as [revision] but expects an object identifier hash directly. *)
let revision_unsafe (hash : Lang.object_id) =
  let url = url (Format.sprintf "/revision/%s/" hash) in
  on_response url (fun response ->
      let field = "directory" in
      match Json.find_string field response with
      | None -> field_not_found field
      | Some dir -> directory_unsafe dir )

(** For a given revision identifier, compute an URL from which the revision can
    be downloaded. *)
let revision id =
  match Lang.get_object_type id with
  | Revision -> revision_unsafe @@ Lang.get_object_id id
  | Content _
  | Release
  | Directory
  | Snapshot ->
    Error "invalid object type (expected Revision)"

(** Same as [release] but expects an object identifier hash directly. *)
let rec release_unsafe (hash : Lang.object_id) =
  let url = url (Format.sprintf "/release/%s/" hash) in

  on_response url (fun response ->
      let field = "target_type" in
      match Json.find_string field response with
      | None -> field_not_found field
      | Some target_type -> (
        let field = "target" in
        match Json.find_string field response with
        | None -> field_not_found field
        | Some target -> begin
          match target_type with
          | "release" -> release_unsafe target
          | "revision" -> revision_unsafe target
          | "content" ->
            (* TODO: get the correct hash type *)
            content_unsafe target ~hash_type:"sha1_git"
          | "directory" -> directory_unsafe target
          | target_type ->
            Error (Format.sprintf "unknown target type: `%s`" target_type)
        end ) )

(** For a given release identifier, compute an URL from which the release can be
    downloaded. *)
let release id =
  match Lang.get_object_type id with
  | Release -> release_unsafe @@ Lang.get_object_id id
  | Content _
  | Revision
  | Directory
  | Snapshot ->
    Error "invalid object type (expected Release)"

(** Same as [snapshot] but expects an object identifier hash directly. *)
let snapshot_unsafe =
  let go_through_objs = function
    | Json.Object o ->
      let rec aux target_type target jsonl =
        match (target_type, target) with
        | Some target_type, Some target -> begin
          match target_type with
          | "revision" -> Some (revision_unsafe, target)
          | "release" -> Some (release_unsafe, target)
          | "content" ->
            (* TODO: fetch the correct hash_type *)
            Some (content_unsafe ~hash_type:"sha1", target)
          | "directory" -> Some (directory_unsafe, target)
          | _ -> None
        end
        | _ -> (
          match jsonl with
          | [] -> None
          | ("target_type", Json.String value) :: r -> aux (Some value) target r
          | ("target", Json.String value) :: r -> aux target_type (Some value) r
          | (_, _) :: r -> aux target_type target r )
      in
      aux None None o
    | _ -> None
  in
  fun (hash : Lang.object_id) ->
    let url = url (Format.sprintf "/snapshot/%s/" hash) in

    on_response url (fun response ->
        let field = "branches" in
        match Json.find_obj field response with
        | None -> field_not_found field
        | Some branch ->
          let requests =
            List.filter_map (fun f -> go_through_objs @@ snd f) branch
          in
          Ok (List.map (fun (f, x) -> f x) requests) )

(** For a given snapshot identifier, compute a list of URL from which the
    snapshot's branches can be downloaded. *)
let snapshot id =
  match Lang.get_object_type id with
  | Snapshot -> snapshot_unsafe @@ Lang.get_object_id id
  | Content _
  | Revision
  | Directory
  | Release ->
    Error "invalid object type (expected Snapshot)"

(** For any object identifier, compute a list of URLs from which the object can
    be downloaded. For all kind of object, the list should contain a single URL
    except for snapshot objects which may lead to a list of many URLs (one URL
    per branch). In the snapshot branch, if a single error is encountered, then
    the result will be an [Error] type with the list of all errors, and no URL
    is returned (even if we succeeded to compute some of them).*)
let any =
  let extract_url = function
    | Error e -> Error [ e ]
    | Ok url -> Ok [ url ]
  in
  fun (identifier : Lang.identifier) : (string list, string list) Result.t ->
    let object_id = Lang.get_object_id identifier in
    match Lang.get_object_type identifier with
    | Lang.Content hash_type ->
      extract_url (content_unsafe ~hash_type object_id)
    | Directory -> extract_url (directory_unsafe object_id)
    | Release -> extract_url (release_unsafe object_id)
    | Revision -> extract_url (revision_unsafe object_id)
    | Snapshot -> (
      match snapshot_unsafe object_id with
      | Error e -> Error [ e ]
      | Ok res -> (
        match
          List.fold_left
            (fun acc r ->
              match acc with
              | Ok url_list -> begin
                match r with
                | Ok url -> Ok (url :: url_list)
                | Error e -> Error [ e ]
              end
              | Error error_list -> begin
                match r with
                | Ok _url -> Error error_list
                | Error e -> Error (e :: error_list)
              end )
            (Ok []) res
        with
        | Ok urls -> Ok (List.rev urls)
        | Error errors -> Ok (List.rev errors) ) )
