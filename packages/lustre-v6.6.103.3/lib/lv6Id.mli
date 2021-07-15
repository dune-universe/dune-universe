(* Time-stamp: <modified the 21/07/2017 (at 15:48) by Erwan Jahier> *)

(**  *)

type t = string (* exported for the debugger *) 
type long = t * t
type pack_name = t

val to_string : t -> string
val of_string : string -> t

val of_long : long -> t
val pack_name_of_string : string -> pack_name
val pack_name_to_string : pack_name -> string
val pack_of_long : long -> pack_name

val string_of_long : bool -> long -> string
val string_of_long_bis : bool -> long -> string
val long_of_string : string -> long

(** To ignore pack name (meaningful when generating ec code for exemple *)
val no_pack_string_of_long : long -> string

val make_long : pack_name -> t -> long

(** lift simple string to long WITH EMPTY PACK *)
val out_of_pack : string -> long

val set_dft_pack_name : pack_name -> unit

(* TODO: a renommer et a abstraire  ??  
   a mettre dans syntaxe.ml ???

   During parsing, we don't know yet what default name we should
   give to the package. Once we know it, we manipulate Lv6Id.t rather than idref

   idref is used to denote user ident, that migth be prefixed
   by the module name or not. One of the first stage of the compiling 
   will consist in transforming those idref (should be called user_id?)
   into Lv6Id.long

*)
type idref = 
    {
      id_pack : string option;
      id_id  : string
    }

val idref_of_string : string -> idref
val make_idref : pack_name -> t -> idref


val string_of_idref : bool -> idref -> string
val raw_string_of_idref : idref -> string
val of_idref : bool -> idref -> t
val to_idref : t -> idref

val name_of_idref : idref -> t
val pack_of_idref : idref -> pack_name option

(** [long_of_idref default_pack_name id_ref] builds a long ident from a 
    AstV6.idref *)
val long_of_idref : idref -> long

val idref_of_long : long -> idref
val idref_of_id : t  -> idref

type clk = long * t
val string_of_clk : clk -> string

val wrap_idref : idref -> string -> string -> idref
