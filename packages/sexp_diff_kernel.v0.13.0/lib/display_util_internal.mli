(** Expose display helpers for use by the incr_dom-friendly library "sexp_diff" *)
module Color : sig
  type t =
    | Red
    | Green
    | Plain
  [@@deriving sexp_of, compare]

  val equal : t -> t -> bool
end

module Line : sig
  type t =
    { color : Color.t
    ; content : string
    }
  [@@deriving fields, sexp_of]

  val to_text
    :  green:(string -> 'a)
    -> red:(string -> 'a)
    -> plain:(string -> 'a)
    -> t
    -> 'a
end

module Display_options : sig
  type t [@@deriving sexp_of]

  val create : ?collapse_threshold:int -> ?num_shown:int -> unit -> t
  val default : t
end

val hide_message : num_hidden:int -> string
val all_hidden_message : string

val display
  :  ?display_options:Display_options.t
  -> Diff.t
  -> on_hidden:(num_hidden:int -> width:int -> 'a)
  -> on_all_hidden:(width:int -> 'a)
  -> on_line_pair:(left:Line.t
                   -> right:Line.t
                   -> left_padding:string
                   -> right_padding:string
                   -> 'a)
  -> 'a list
