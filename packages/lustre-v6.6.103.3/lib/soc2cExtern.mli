(* Time-stamp: <modified the 13/03/2020 (at 11:32) by Erwan Jahier> *)


(* [gen_files main_soc licprg cfile hfile] returns a pair of bool that
   states if an extern C file (LHS) and an extern header file (RHS)
   are necessary to compile the main soc. 

   A C file is necessary if an extern step exists.
   A header file in necessary if an extern step, const, or types exists.

   As a side effect, and as its name suggests, this function also
   generates files, cfile and hfile, if they do not already exist in
   the current directory.

   nb: In order to ease the non reg tests, I've chosen to define
   extern types as int, and empty step function that compiles, and to
   add "XXX" comments where fixes are required. Is it really a good
   idea ?
*)
val gen_files : Soc.t -> Soc.tbl -> LicPrg.t -> string -> string -> string -> bool * bool
                
(* val cpy_declaration : LicPrg.t -> string *)
val const_declaration : LicPrg.t -> string

(* ext type getters for the _loop.c file *)
val gen_getters : string -> LicPrg.t -> Soc.t -> string
