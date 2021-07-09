(* Time-stamp: <modified the 28/08/2019 (at 16:56) by Erwan Jahier> *)

(* class type ocaml_reactive_machine = *)
(*    object  *)
(*      method step : Data.subst list -> Data.subst list *)
(*      method kill : string -> unit *)
(*    end;; *)
(* type vars = (Data.ident * Data.t) list *)

let myexit _i = failwith "error in rdbg (OcamlRun.make_ocaml)" 

let (make_ocaml : string -> RdbgPlugin.t) =
  fun cmxs ->
    let _ =
      Dynlink.allow_unsafe_modules true; 
      try Dynlink.loadfile cmxs
      with Dynlink.Error msg ->
        Printf.eprintf "\n*** error in rdbg.cmxa (Dynlink.loadfile %s).\n*** %s.\n"
          cmxs (Dynlink.error_message msg);
        flush stderr;
        myexit 2
    in
    let res = OcamlRM.get_plugin cmxs in
    Printf.eprintf "rdbg:  %s file loaded.\n" cmxs;
    res
    (* cf 
       http://blog.rastageeks.org/ocaml/article/dynlink-as-dlopen
       http://okmij.org/ftp/ML/first-class-modules/ 
    *)
    
let (make : string -> RdbgPlugin.t) = make_ocaml
