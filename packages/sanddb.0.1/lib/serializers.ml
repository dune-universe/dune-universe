module type Json_Serializer = sig
  type t
  val t_of_string : string -> t
  val string_of_t : ?len:int -> t -> string
end;;

module type Biniou_Serializer = sig
  type t
  val t_of_string : ?pos:int -> string -> t
  val string_of_t : ?len:int -> t -> string
end;;