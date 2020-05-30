val encode : string -> string
(** encode "Today's special" = "Today%27s%20special" *)

val make_query : (string * string) list -> string
(** make_query ["k", "v"; "k2", "v2"] = "k=v&k2=v2" *)
