
type 'a part = {
  p_cardinal : Z.t ;
  compute : Z.t -> 'a ;
}

val empty_part : 'a part
val uninitialized_part : 'a part

val part_from_list : 'a list -> 'a part

val map_part  : ('a -> 'b) -> 'a part -> 'b part

val union_parts   : 'a part list -> 'a part
val product_parts : 'a part list -> ('a list) part
