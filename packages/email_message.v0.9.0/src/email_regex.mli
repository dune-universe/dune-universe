(* Regex module so we can change the implementation easily *)
type t

module Template : sig
  type t
end

module type Creators_intf = sig
  (* Creates regexes using several combinations of the PCRE options.
     Raises exceptions if string is not a valid Regexp *)
  val create : string -> t
  val create_i : string -> t
  val create_im : string -> t
  val create_m : string -> t

  module Template : sig
    type t = Template.t
    type regex
    val create : regex:regex -> template:string -> t
  end with type regex := t
end

module type Accessors_intf = sig
  module Infix : sig
    val (=~) : string -> t -> bool
  end

  module Match : sig
    type t
    val by_index : t -> int -> string
    val by_name : t -> string -> string
  end

  module Template : sig
    type t
    val apply : t -> string -> string;;
  end with type t := Template.t

  val matches : t -> string -> bool;;
  val apply : t -> string -> Match.t option
  val split_at : t -> string -> string list;;

end

module Accessors : Accessors_intf
module Creators : Creators_intf



