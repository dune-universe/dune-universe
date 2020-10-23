type t = {
  days : int;
  hours : int;
  minutes : int;
  seconds : int;
}

val zero : t

val of_seconds : int64 -> (t, unit) result

val to_seconds : t -> int64

val normalize : t -> t

val duration_expr_parser : (t, unit) MParser.t

val of_string : string -> (t, string) result

module To_string : sig
  val human_readable_string_of_duration : t -> string
end
