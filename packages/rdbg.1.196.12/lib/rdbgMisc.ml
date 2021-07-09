(* Time-stamp: <modified the 10/02/2016 (at 14:14) by Erwan Jahier> *)


(****************************************************************************)
let mygetenv_def def x = 
  let x = 
    match Sys.os_type with
      | "Win32" -> (x^"_DOS")
      | _ ->  x
  in
    try Unix.getenv x 
    with Not_found -> def

(* try to compile a C file into a dro file *)
let (c2dro: string -> bool) =
  fun cfile ->
    let base = Filename.chop_extension cfile in
    let dro  = base ^ ".dro" in
    try
      let cc = try mygetenv_def "gcc" "CC" with  _ -> "gcc" in
      let args = [cfile;  "-fPIC"; "-shared"; "-o"; dro ] in
	   let res = (Mypervasives.my_create_process 
                   ~std_out:Unix.stderr ~wait:true cc args)
      in
      if res <> Mypervasives.OK  
         && (* well, try with Sys.command that sometimes works 
               better wrt path issues *) 
           (Sys.command (String.concat " " (cc::args)) <> 0)

      then (
        print_string "c2dro failed to call the c compiler\n";
        print_string "Try to define the CC env variable.\n";
        flush stdout;
        false
      ) else true
    with e -> 
      print_string (Printexc.to_string e^"\nTry to define the CC env variable.\n");
      flush stdout;
      false
