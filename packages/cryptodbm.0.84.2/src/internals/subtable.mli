open Types
open Operations
open Cipher
open Kinds

(* Type of a subtable. 'a is either Types.read of Types.full. *)
type 'a sub

(* Mimics an empty subtable in read mode. 
 * empty subtable_name subtable_number *)
val empty : string -> int -> read sub

(* Opens/create a subtable
 * name: subtable name, used to report error messages precisely.
 * salt: the table salt
 * how: table password (salted, strengthened) and subtable password (plain: not salted, not strengthen). *)
val open_read : 'a handler -> name:string -> subt:int -> iterations:int -> how:(passwd * string) howstored -> 
  signwd:string -> read sub

val open_full : full handler -> name:string -> subt:int -> iterations:int -> how:(passwd * string) howstored -> 
   signwd:string -> max_extra_key:int -> max_extra_data:int -> full sub

val open_append : full handler -> name:string -> subt:int -> iterations:int -> how:(passwd * string) howstored ->
  signwd:string -> check_signature:bool -> full sub

val close : 'a sub -> unit

(* Empty this subtable content. *)
val clear : full sub -> unit

(* Check that the subtable is not closed.
 * Sign if necessary. Do nothing otherwise. *)
val sign : 'a sub -> unit

val remove_signature : full sub -> unit

val get_number : 'a sub -> int
val get_name   : 'a sub -> string

val add : ?may_overwrite:bool -> full sub -> key:string -> data:string -> unit

(* @raise Error (Unbound (key, loc)) 
 * @raise Error (Is_Closed loc) *)
val find : 'a sub -> string -> string
val delete : full sub -> string -> unit

val is_bound : 'a sub -> string -> bool

val iter : 'a sub -> (string -> string -> unit) -> unit

(* Iterate over user keys. *)
val iterkey : 'a sub -> (string -> unit) -> unit

val fold : 'a sub -> 'b -> (string -> string -> 'b -> 'b) -> 'b

(* Raises an error located in this subtable. *)
val error : (error_location -> error) -> 'a sub -> 'b
