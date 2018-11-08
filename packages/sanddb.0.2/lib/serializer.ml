(** [Sanddb.Serializer] contains the different kind of serializers that are possible with SandDB.*)

module type Generic_serializer = sig
  type t
  val t_of_string : string -> t
  val string_of_t :  t -> string
end;;

module type Json_serializer = sig
  type t
  val t_of_string : string -> t
  val string_of_t : ?len:int -> t -> string
end;;

module type Biniou_serializer = sig
  type t
  val t_of_string : ?pos:int -> string -> t
  val string_of_t : ?len:int -> t -> string
end;;