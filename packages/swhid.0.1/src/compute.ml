(** This module provides various functions to compute the swhid of a given
    object. Supported objects are [content], [directory], [release], [revision]
    and [snapshot]. The origins and visits objects are not supported. To learn
    more about the different object types and identifiers see the
    {{:https://docs.softwareheritage.org/devel/swh-model/data-model.html#software-artifacts}
    software heritage documentation}.*)

open Lang

(** The type for directory entries list, needed to compute directories
    identifiers. *)
type directory_entry =
  { typ : string  (** e.g. "file", "dir" or "rev" *)
  ; permissions : int
  ; name : string
  ; target : object_id
  }

(** The type for dates, needed to compute releases and revisions identifiers. *)
type date =
  { timestamp : int
  ; tz_offset : int
  ; negative_utc : bool
  }

(** [content_identifier s] computes the swhid for the [s] content. [s] is the
    raw content of a file as a [string].

    E.g. [content_identifier "_build\n"] is the swhid of this library's
    [.gitignore] file. *)
let content_identifier content : Lang.identifier option =
  let git_object = Git.object_from_contents (Content "sha1_git") content in
  Git.object_to_swhid git_object [] Lang.content

(** [directory_identifier entries] compute the swhid for the [entries]
    directory. [entries] is a list of [Lang.directory_entry] where each element
    points to another object (usually a file content or a sub-directory).

    E.g.
    [directory_identifier \[ { typ = "file"
                                 ; permissions = 33188
                                 ; name = "README"
                                 ; target = "37ec8ea2110c0b7a32fbb0e872f6e7debbf95e21"
                                 }\]]
    is the swhid of a directory which has a single file [README] with
    permissions 33188 and whose core identifier from [content_identifier] is
    [37ec8ea2110c0b7a32fbb0e872f6e7debbf95e21]. *)
let directory_identifier entries : Lang.identifier option =
  List.iter
    (fun entry ->
      if Lang.object_id_invalid entry.target then
        raise @@ Invalid_argument "target must be of length 40" )
    entries;
  let entries =
    List.sort
      (fun entry1 entry2 ->
        String.compare
          ( if entry1.typ = "dir" then
            entry1.name ^ "/"
          else
            entry1.name )
          ( if entry2.typ = "dir" then
            entry2.name ^ "/"
          else
            entry2.name ) )
      entries
  in
  let content =
    Format.asprintf "%a"
      (Format.pp_print_list
         ~pp_sep:(fun _fmt () -> ())
         (fun fmt entry ->
           Format.fprintf fmt "%o %s%c%s" entry.permissions entry.name '\x00'
             (Git.id_to_bytes entry.target) ) )
      entries
  in
  let git_object = Git.object_from_contents Directory content in
  Git.object_to_swhid git_object [] Lang.directory

(** [release_identifier target target_type name ~author date ~message] computes
    the swhid for a release object poiting to an object of type [target_type]
    whose identifier is [target], the release having name [name], author
    [~author] and has been published on [date] with the release message
    [~message]. *)
let release_identifier target target_type name ~author date ~message :
    Lang.identifier option =
  if Lang.object_id_invalid target then
    raise @@ Invalid_argument "target must be of length 40";

  let buff = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buff in

  Format.fprintf fmt "object %s%ctype %s%ctag %s%c" target '\n'
    (Git.target_type_to_git target_type)
    '\n' (Git.escape_newlines name) '\n';

  begin
    match author with
    | None -> ()
    | Some author ->
      Format.fprintf fmt "tagger %a%c" Git.format_author_data
        ( Git.escape_newlines author
        , Option.map (fun o -> (o.timestamp, o.tz_offset, o.negative_utc)) date
        )
        '\n'
  end;

  begin
    match message with
    | None -> ()
    | Some message -> Format.fprintf fmt "%c%s" '\n' message
  end;

  Format.pp_print_flush fmt ();

  let content = Buffer.contents buff in

  let git_object = Git.object_from_contents Release content in

  Git.object_to_swhid git_object [] Lang.release

(** [revision dir parents ~author ~author_date ~committer ~committer_date extra_headers message]
    computes the swhid for a revision object whose directory has id [dir] and
    whose parents has ids [parents] which was authored by [~author] on
    [~author_date] and committed by [~committer] on [~committer_date] with extra
    headers [extra_headers] and message [message]. *)
let revision_identifier directory parents ~author ~author_date ~committer
    ~committer_date extra_headers message : Lang.identifier option =
  if List.exists Lang.object_id_invalid (directory :: parents) then
    raise
    @@ Invalid_argument "target (directory and parents) must be of length 40";

  let buff = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buff in

  Format.fprintf fmt "tree %s%c" directory '\n';

  List.iter (fun parent -> Format.fprintf fmt "parent %s%c" parent '\n') parents;

  Format.fprintf fmt "author %a%c" Git.format_author_data
    ( Git.escape_newlines author
    , Option.map
        (fun o -> (o.timestamp, o.tz_offset, o.negative_utc))
        author_date )
    '\n';

  Format.fprintf fmt "committer %a%c" Git.format_author_data
    ( Git.escape_newlines committer
    , Option.map
        (fun o -> (o.timestamp, o.tz_offset, o.negative_utc))
        committer_date )
    '\n';

  Array.iter
    (fun (k, v) -> Format.fprintf fmt "%s %s%c" k (Git.escape_newlines v) '\n')
    extra_headers;

  begin
    match message with
    | None -> ()
    | Some message -> Format.fprintf fmt "%c%s" '\n' message
  end;

  Format.pp_print_flush fmt ();

  let content = Buffer.contents buff in

  let git_object = Git.object_from_contents Revision content in

  Git.object_to_swhid git_object [] Lang.revision

(** [snapshot_identifier branches] computes the swhid of the snapshot made of
    branches [branches] where [branches] is a list of branch elements. Each
    branch is of the form [name, target] where [name] is the name of the branch
    and where [target] is a pair made of the identifier of the branch and its
    type. *)
let snapshot_identifier (branches : (string * (string * string) option) list) :
    Lang.identifier option =
  let branches =
    List.sort
      (fun (name1, _target) (name2, _target) -> String.compare name1 name2)
      branches
  in
  let buff = Buffer.create 512 in
  let fmt = Format.formatter_of_buffer buff in
  List.iter
    (fun (branch_name, target) ->
      let target, target_type, target_id_len =
        match target with
        | None -> ("", "dangling", 0)
        | Some (target, target_type) -> (
          match target_type with
          | "content"
          | "directory"
          | "revision"
          | "release"
          | "snapshot" ->
            (Git.id_to_bytes target, target_type, 20)
          | "alias" -> (target, "alias", String.length target)
          | target_type ->
            raise
            @@ Invalid_argument
                 (Format.sprintf
                    "invalid target type: `%s` (Compute.snapshot_identifier)"
                    target_type ) )
      in
      Format.fprintf fmt "%s %s%c%d:%s" target_type branch_name '\x00'
        target_id_len target )
    branches;
  Format.pp_print_flush fmt ();
  let content = Buffer.contents buff in
  let git_object = Git.object_from_contents_strtarget "snapshot" content in
  Git.object_to_swhid git_object [] Lang.snapshot
