(** Internal use only *)
type ('a,'re) t = {
  string : string;
  re : 're;
  binder : left:string -> right:string -> last:string option -> string option array -> 'a;
}

(** Internal use only *)
class virtual group : (string * int) list -> left:string -> right:string -> last:string option -> string option array -> object
  method _group : int -> string
  method _group_opt : int -> string option
  method _groups : string array
  method _groups_opt : string option array
  method _last : string
  method _last_opt : string option
  method _left : string
  method _named_group : string -> string
  method _named_group_opt : string -> string option
  method _named_groups : (string * string) list
  method _named_groups_opt : (string * string option) list
  method _right : string
  method _unsafe_group : int -> string
  method _unsafe_group_opt : int -> string option
end

(** Internal use only *)
val create :
  string ->
  're ->
  (left:string -> right:string -> last:string option -> string option array -> 'a) ->
  ('a, 're) t

(** Internal use only *)
val make_group_obj :  ('a, 're) t -> string -> string option array -> string -> 'a
