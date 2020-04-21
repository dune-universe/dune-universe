type t
val count: t -> int
val has_locals: t -> bool
val find:  string -> t -> int list
val empty: t
val add_unnamed: t -> t
val add_local:  string -> t -> t
val add_global: string -> t -> t
