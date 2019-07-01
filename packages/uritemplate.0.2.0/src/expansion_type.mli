type t =
  | Simple (** \{var\} *)
  | Reserved (** \{+var\} *)
  | Fragment (** \{#var\} *)
  | Dot (** \{.var\} *)
  | PathSegment (** \{/var\} *)
  | PathParameter (** \{;var\} *)
  | FormQuery (** \{?var\} *)
  | FormQueryContinuation (** \{&var\} *)
(** The type of expansion that *)

val expansion_type_of_string : string -> t
(** Returns the expasion type represented by the give string (see {!type:t}). Or Simple if not recognised *)

val string_of_expansion_type : t -> string
(** [string_of_expansion_type ex] returns the character that represents the expansion type [ex] or any empty string for {!constructor:Simple} *)

val separator_for_expansion_type : t -> char
(** [separator_for_expansion_type ex] returns the character that seperates multiple expanded variables as specified by {{: https://tools.ietf.org/html/rfc6570#section-3.2.1} Section 3.2.1} *)
