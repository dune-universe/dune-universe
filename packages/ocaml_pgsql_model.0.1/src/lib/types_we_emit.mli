module Types_we_emit : sig
  type t =
    | Bignum
    | CoreInt32
    | CoreInt64
    | Float
    | Date
    | Time
    (*===TODO===for the really paranoid; introduce a type that extends string and is length aware, 
      and never permits truncation when storing to the db, although would have to handle runtime exceptions*)
    | String
    | Bool
	[@@deriving show]

  val to_string : t:t -> is_nullable:bool -> string
  val converter_of_string_of_type : is_optional:bool -> t:t -> fieldname:string -> string
  val converter_to_string_of_type : is_optional:bool -> t:t -> string
end 
                         
