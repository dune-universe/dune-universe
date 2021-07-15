
(*
   Idee:
   - regrouper ici tout ce qui permet de faire du verbose/debug 
     et qui est diffus et pas toujours homogene.
   - typiquement tous les print et to_string dont on a besoin
     dans les Verbose
   - autant que faire se peut, deux sortes de fonction :
     * s_toto : toto -> string
     * p_toto : toto -> unit
       qui ecrit sur stderr par defaut, sur une ligne sans rc autant que possible

   - a terme, mettre tout ce qu'il faut ici !
*)

(* affichage basique *)
let cr ?(oc=stderr) () : unit = output_string oc "\n"
let pf ?(oc=stderr) s = Printf.kprintf (fun t -> output_string oc t; flush oc) s

(* lexical *)
let s_lxm : Lxm.t -> string = Lxm.short_details

(* ast *)
let p_val_exp ?(oc=stderr) (ve:AstCore.val_exp) : unit = AstV6Dump.print_short_val_exp oc ve

(* lic = ast semantique *)
let s_const_eff      : Lic.const      -> string = LicDump.string_of_const_eff false
let s_const_eff_list : Lic.const list -> string = LicDump.string_of_const_eff_list false
