(* Time-stamp: <modified the 21/07/2020 (at 17:29) by Erwan Jahier> *)

type sl = Data.subst list
type vars = (string * string) list

open RdbgArg

let debug_msg msg =     
  if args.debug_rdbg then (output_string stdout ("RifRun: "^msg); flush stdout)

let (step_channel : string -> in_channel -> out_channel ->
     (Data.ident * Data.t) list -> (Data.ident * Data.t) list ->
     Data.subst list -> Data.subst list option) =
  fun label ic oc in_vars out_vars sl ->
    let my_string_of_float v = Mypervasives.my_string_of_float v args.precision in
    let in_vals_str =
      List.fold_left
        (fun acc (name, _) ->
           try
             let value = List.assoc name sl in
             acc ^ " "^ (Data.val_to_rif_string my_string_of_float value)
           with Not_found -> acc
        )
        ""
        in_vars
    in
    try 
      let res =
        debug_msg  (label ^ " receives '" ^ in_vals_str ^"'.\n");
        output_string oc (in_vals_str ^"\n");
        flush oc;
        Printf.printf "on a single line, enter %i inputs:%!" (List.length out_vars);
        RifIO.read ~debug:args.debug_rdbg
          ~label:("read the result of "^label) 
          ic None out_vars
      in
      Some res
    with
      RifIO.Reset -> None
      
open RdbgPlugin

let (make :  vars -> vars ->  (string -> unit) * (sl -> sl option)) =
  fun rif_inputs rif_outputs -> 
  let kill _msg = () in
  let cmd_oc = Unix.out_channel_of_descr Unix.stdout in
  let cmd_ic = Unix.in_channel_of_descr Unix.stdin in
  let rif_inputs  = List.map (fun (n,t) -> n,Data.type_of_string t) rif_inputs in
  let rif_outputs = List.map (fun (n,t) -> n,Data.type_of_string t) rif_outputs in
  let step = 
    if args.debug_rdbg then (Printf.fprintf stderr "\nStep stdio.\n" ; flush stderr);
    step_channel "stdio" cmd_ic cmd_oc rif_inputs rif_outputs 
  in
  kill,step
