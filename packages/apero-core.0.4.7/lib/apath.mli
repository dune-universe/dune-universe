
module Path : sig
  type t
  val of_string : string -> t
  (** [of_string s] returns [s] as a Path if it's valid. Otherwise it raises an [Exception].
      Note that the Path's string is sanitized (i.e. it's trimmed meaningless '/' are removed) *)
  val of_string_opt : string -> t option
  (** [of_string_opt s] returns [Some p] if [s] is a valid path. Otherwise returns [None].
      Note that the Path's string is sanitized (i.e. it's trimmed and meaningless '/' are removed) *)
  val to_string : t -> string
  (** [to_string path] returns the [path] as a string. *)

  val length : t -> int
  (** [length p] returns the number of characters of path [p] *)
  val compare : t -> t -> int
  (** The comparison function for paths, with the same specification as [Pervasives.compare] *)
  val equal : t -> t -> bool 
  (** The equal function for paths. *)

  val is_relative : t -> bool
  (** [is_relative p] return true if the Path [p] is relative (i.e. it's first character is not '/') *)
  val add_prefix : prefix:t -> t -> t
  (** [add_prefix prefix p] return a new Path made of [prefix]/[p] *)
  val is_prefix : affix:t -> t -> bool
  (** [is_prefix affix p] returns true if [affix] is a prefix of [p] *)
  val remove_prefix : int -> t -> t
  (** [remove_prefix l p] removes the [l] first characters from Path [p] and returns the remaining as a non-absolute Path *)
end [@@deriving show]


module PathExpr : sig 
  type t

  val of_string : string -> t
  (** [of_string s] returns [s] as a PathExpr if it's valid. Otherwise it raises an [Exception].
      Note that the expression is sanitized (i.e. it's trimmed meaningless '/' are removed) *)
  val of_string_opt : string -> t option
  (** [of_string_opt s] returns [Some pe] if [s] is a valid path expression. Otherwise returns [None].
      Note that the expression is sanitized (i.e. it's trimmed and meaningless '/' are removed) *)
  val to_string : t -> string
  (** [to_string e] return the expression [e] as a string *)
  val of_path : Path.t -> t
  (** [of_path p] returns an expression equal to [p] *)

  val length : t -> int
  (** [length e] returns the number of characters of expression [e] *)
  val compare : t -> t -> int
  (** The comparison function for expressions, with the same specification as [Pervasives.compare] *)
  val equal : t -> t -> bool 
  (** The equal function for expressions. *)

  val is_relative : t -> bool
  (** [is_relative e] return true if the expression [e] is relative (i.e. it's first character is not '/') *)
  val get_prefix : t -> Path.t
  (** [get_prefix e] return the longest prefix of [e] that is a Path (i.e. without any wildcard) *)
  val add_prefix : prefix:Path.t -> t -> t
  (** [add_prefix prefix e] return a new expression made of [prefix]/[e]. *)
  val remove_prefix : int -> t -> t
  (** [remove_prefix l e] removes the [l] first characters from expression [e] and returns the remaining as a non-absolute PathExpr *)

  val is_unique : t -> bool
  (** [is_unique e] returns true if the expression [e] doesn't contains any wildcard ('*'). *)
  val as_unique_path : t -> Path.t option
  (** [as_unique_path e] returns the expression [e] as Some Path.t if it doesn't contain any wildcard ('*').
      It returns None otherwise. *)

  val is_matching_path : Path.t -> t -> bool
  (** [is_matching_path p e] returns true if the expression [e] fully matches the path [p]. *)

  val intersect : t -> t -> bool 
  (** [intersect e1 e2] returns true if the intersection of expressions [e1] and [e2] is not empty. 
      I.e. if it exists a path p that matches both expressions [e1] and [e2]. *)
  val includes : subexpr:t -> t -> bool
  (** [includes subexpr e] returns true if the expression [e] includes the expression [subexpr].
      I.e. if [subexpr] matches a path p, [e] also matches p. *)

  val longest_matching_part : Path.t -> t -> t
  (** [longest_matching_part path e] returns the longest prefix within [e] that matches [path],
      or an empty PathExpr no matching prefix of [path] can be found in [e]. *)

  val remaining_after_match : Path.t -> t -> t option
  (** [remaining_after_match path e] tries to find the longest prefix within [e] that matches [path] and returns the remaining part of [e] when removing this prefix.
      If matching prefix of [path] can be found in [e], None is returned. *)

end [@@deriving show]