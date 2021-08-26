(** The kinds of objects represented by swhids, see the
    {{:https://docs.softwareheritage.org/devel/swh-model/data-model.html#software-artifacts}
    software heritage model documentation}. *)
type object_type =
  | Content of string
      (** The string parameter is the hash function name used for the
          computation, defaults to "sha1_git" (in most cases, you don't care
          about it) *)
  | Directory
  | Release
  | Revision
  | Snapshot

(** Must be of length 40 and made only of hexadecimal characters. *)
type object_id = string

(** The scheme version, the object type and the object identifier. *)
type identifier_core = int * object_type * object_id

(** See
    {{:https://docs.softwareheritage.org/devel/swh-model/persistent-identifiers.html#qualifiers}
    swh documentation about qualifiers}.*)
type context_qualifier =
  | Anchor of identifier_core
      (** a designated node in the Merkle DAG relative to which a path to the
          object is specified, as the core identifier of a directory, a
          revision, a release or a snapshot *)
  | Origin of string
      (** the software origin where an object has been found or observed in the
          wild, as an URI *)
  | Path of string
      (** the absolute file path, from the root directory associated to the
          anchor node, to the object; when the anchor denotes a directory or a
          revision, and almost always when it’s a release, the root directory is
          uniquely determined; when the anchor denotes a snapshot, the root
          directory is the one pointed to by HEAD (possibly indirectly), and
          undefined if such a reference is missing *)
  | Visit of identifier_core
      (** the core identifier of a snapshot corresponding to a specific visit of
          a repository containing the designated object *)

(** See
    {{:https://docs.softwareheritage.org/devel/swh-model/persistent-identifiers.html#qualifiers}
    swh documentation about qualifiers}.*)
type qualifier =
  | Context of context_qualifier  (** either a context *)
  | Fragment of (int * int option)  (** or a fragment (a line number or two) *)

(** The type for full swhids. *)
type identifier = identifier_core * qualifier list

(** Helper to build an [object_type] from a [string].*)
let object_type_of_string = function
  | "snp" -> Some Snapshot
  | "rel" -> Some Release
  | "rev" -> Some Revision
  | "dir" -> Some Directory
  | "cnt" -> Some (Content "sha1_git")
  | _s -> None

(** Checks if an [object_id] is invalid, i.e. if it's not of length 40 or if it
    contains non-hexadecimal characters. *)
let object_id_invalid (target : object_id) =
  String.length target <> 40
  ||
  try
    String.iter
      (function
        | 'a' .. 'f'
        | '0' .. '9' ->
          ()
        | _invalid_char -> raise Exit )
      target;
    false
  with
  | Exit -> true

(** Helper function to build an [object_id] from a [string] that will return
    [None] if the string isn't valid according to [object_type_is_invalid] *)
let object_id_from_string (s : string) : object_id option =
  if object_id_invalid s then
    None
  else
    Some s

(** Builds a swhid of kind [content] from it's core id and a list of qualifiers.
    The given hash defaults to ["sha1_git"] and you shouldn't care about it in
    most cases. *)
let content ?(hash_type = "sha1_git") id qualifiers : identifier =
  ((1, Content hash_type, id), qualifiers)

(** Builds a swhid of kind [directory] from it's core id and a list of
    qualifiers. *)
let directory id qualifiers : identifier = ((1, Directory, id), qualifiers)

(** Builds a swhid of kind [snapshot] from it's core id and a list of qualifiers *)
let snapshot id qualifiers : identifier = ((1, Snapshot, id), qualifiers)

(** Builds a swhid of kind [revision] from it's core id and a list of qualifiers *)
let revision id qualifiers : identifier = ((1, Revision, id), qualifiers)

(** Builds a swhid of kind [release] from it's core id and a list of qualifiers *)
let release id qualifiers : identifier = ((1, Release, id), qualifiers)

(** Extract the core id from a given identifier. *)
let get_object_id
    (((_scheme_version, _object_type, object_id), _qualifiers) : identifier) :
    object_id =
  object_id

(** Extract the object type from a given identifier. *)
let get_object_type
    (((_scheme_version, object_type, _object_type), _qualifiers) : identifier) :
    object_type =
  object_type

(** Exception raised by the parser when e.g. the scheme version is unknown. *)
exception Parser_error of string
