module Display_options : sig
  type t [@@deriving sexp_of]

  (** Whenever there is a sequence of [collapse_threshold] or more unchanged lines,
      we collapse them and only show the first [num_shown] and the last [num_shown] of
      these lines. *)

  val create : ?collapse_threshold:int -> ?num_shown:int -> unit -> t
end

(** [display_as_plain_string] displays the diff as a string.  Here is a possible output of
    this function:

    {v
               (                         (
              - (apple 123)             + (apricot 321)
                (banana 1000)             (banana 1000)
                (banana 1000)             (banana 1000)
                (banana 1000)             (banana 1000)
                          ...6 unchanged lines...
                (banana 1000)             (banana 1000)
                (banana 1000)             (banana 1000)
                (banana 1000)             (banana 1000)
                                        + (durian 1234)
                (carrot                   (carrot
              -  -1                     +  42
                ))                        ))
     v}

    As you can see, deletions are marked by '-' and additions are marked by '+'.
*)
val display_as_plain_string : ?display_options:Display_options.t -> Diff.t -> string

val display_as_string_with_custom_formatting
  :  ?display_options:Display_options.t
  -> Diff.t
  -> green:(string -> string)
  -> red:(string -> string)
  -> plain:(string -> string)
  -> string

(** [display_with_ansi_colors] displays the same string as [display_as_plain_string], but
    deletions and additions are marked by red and green text rather than '-' and '+'.
*)
val display_with_ansi_colors : ?display_options:Display_options.t -> Diff.t -> string
