open Types
open Operations

(*** Compute signatures ***)

type signature

(* Compute a subtable signature. Only user bindings are used to compute the signature. They are not decrypted. 
 * The table password is needed to find the subtable bindings. The subtable password is not needed. *)
val subtable_signature : 'a handler -> subtable_salt:string -> passwd:Cipher.passwd -> subt:int -> signwd:Cipher.passwd -> signature

(* Compute and write a subtable signature. 
 * The key_kind is needed to store the signature. *)
val sign_subtable : full handler -> subtable_salt:string -> passwd:Cipher.passwd -> Kinds.key_kind -> subt:int -> signwd:Cipher.passwd -> unit

(* @raise Error (Unbound (_, Any)) *)
val read_subtable_signature : 'a handler -> Kinds.key_kind -> subt:int -> signature

(* Compute the whole table signature. All bindings are used. 
 * No password is required. *)
val table_signature : 'a handler -> table_salt:string -> signwd:Cipher.passwd -> signature

(* @raise Error (Unbound (_, Any)) *)
val read_table_signature : 'a handler -> signature

(* Compute and write the table signature. *)
val sign_table : full handler -> table_salt:string -> signwd:Cipher.passwd -> unit

(* Remove signatures *)
val remove_table_signature : full handler -> unit
val remove_subtable_signature : full handler -> Kinds.key_kind -> subt:int -> unit

(* Check signature equality *)
val equal : signature -> signature -> bool



