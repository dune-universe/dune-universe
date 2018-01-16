type tag = 
  | Abstract
  | Closure
  | Custom
  | Double
  | Double_array
  | Forward
  | Infix
  | Int (* this is a virtual one *)
  | Lazy
  | No_scan
  | Object
  | Out_of_heap
  | String
  | Unaligned
  | Unknown of int 

val parse_tag : int -> tag

val tag_name : tag -> string

val dump : Obj.t -> unit
