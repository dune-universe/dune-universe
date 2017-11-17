open Kinds
open Types

(* Note: key_kinds must be already salted. *)

(* Handler to the lower-level database. 
 * 'a is a phantom type : read or full. *)
type 'a handler

(* Root file name (without suffixes). *)
val get_rootfile : 'a handler -> string

val cast : 'a handler -> read handler

(* @raise File_not_found *)
val open_read : file:string -> read handler

(* @raise File_not_writeable, File_overwrite *)
val open_full : ?overwrite:bool -> file:string -> perm:int -> full handler

(* @raise File_not_appendable *)
val open_append : file:string -> full handler

(* @raise Error (Is_Closed Any) *)
val close : 'a handler -> unit
val flush : ?backup:bool -> ?backup_name: string -> full handler -> unit

(* Get the data associated to the given key.
 * @raise Error Unbound (key, Any)  if the key is not bound or if the password is wrong. 
 * @raise Error (Bad_password Any)  if the password is wrong. Most often, Unbound will be raised, though. *)
val get : 'a handler -> key_kind -> key:string -> string

(* Get encrypted data given an encrypted key *)
val get_encrypted : 'a handler -> encoded_key -> encoded_data

(* Adds a new binding to the database.
 * @raise Error (Overwrite (key, Any) *)
val add : ?may_overwrite:bool -> full handler -> key_kind -> max_extra_data:int -> key:string -> data:string -> unit

(* Removes a binding, given its (encrypted) key.
 * @raise Error (Unbound (key, Any))
 * @raise Error (Is_Closed Any) *)
val remove : full handler -> key_kind -> key:string -> unit
val remove_encrypted : full handler -> Kinds.encoded_key -> unit

(* Iterate over all keys of the database that are not encrypted by a subtable-specific password. 
 * passwd is the table password, or empty password if unknown (in which case, only uncrypted bindings are seen). 
 * @raise Error (Is_Closed Any) *)
val iter_uncrypted : 'a handler -> Cipher.passwd -> (location -> string -> unit) -> unit

(* Iterate over all keys of the given subtable: builtin and user.
 * passwd is the table password, or empty password if unknown (in which case, the subtable must be uncrypted).
 * subpass is the subtable password (empty password if none).
 * @raise Error (Is_Closed Any) *)
val iter_subtable : 'a handler -> Cipher.passwd -> subt:int -> subpass:Cipher.passwd -> (location -> string -> unit) -> unit

(* Similar, but keys are kept encrypted.
 * @raise Error (Is_Closed Any) *)
val iter_subtable_encrypted : 'a handler -> Cipher.passwd -> subt:int -> (location -> encoded_key -> unit) -> unit

(* Iterate over all bindings, in encrypted form.
 * @raise Error (Is_Closed Any) *)
val iter_all : 'a handler -> (encoded_key -> encoded_data -> unit) -> unit

