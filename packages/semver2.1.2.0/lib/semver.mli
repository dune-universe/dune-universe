
(** Semantic version parsing and comparator following 2.0.0 specs.
    <https://semver.org/#semantic-versioning-200>
*)

(**
   A semantic version is composed of numeric major, minor and patch versions,
   optionnaly followed by a list of prerelease identifiers and a list of build identifiers.
*)
type t = private {
  major: int;
  minor: int;
  patch: int;
  prerelease: string list;
  build: string list;
}

(** Build a valid version from parts. Require positive values for major, minor and patch params. *)
val from_parts : int -> int -> int -> string list -> string list -> t option

(** Convert a version to its string representation. *)
val to_string : t -> string

(** Parse a string version, returning None if format is invalid. *)
val of_string : string -> t option

(** Check format validity. *)
val is_valid : string -> bool

(** Compare versions, returning 0 if equal, -1 if first has lower precedence, 1 otherwise. *)
val compare : t -> t -> int

(** Return true if first has lower precedence than second. *)
val less_than : t -> t -> bool

(** Return true if first has higher precedence than second. *)
val greater_than : t -> t -> bool

(** Return whether versions are equal. *)
val equal : t -> t -> bool

(** Pretty print version. *)
val pp : Format.formatter -> t -> unit
